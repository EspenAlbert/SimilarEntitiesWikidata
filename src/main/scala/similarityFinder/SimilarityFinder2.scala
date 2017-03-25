package similarityFinder

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Merge, Partition, RunnableGraph, Sink, Source, Unzip, UnzipWith, Zip}
import core.feature.Feature
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF
import core.strategies._
import shapeless.ops.hlist.ZipWith
import similarityFinder.displayer.Displayer
import similarityFinder.ranker.SimilarEntity

import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable
import scala.collection.parallel.mutable.{ParHashMap, ParIterable}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import GraphDSL.Implicits._
object SimilarityFinder2 {
  val thresholdStrategyWeight = 3.25
  val ENTITIES_AFTER_PRUNING = 1000
  val thresholdCountValueMatch = 5000
  def isACheapStrategy(strategy: Strategy) : Boolean= {
    strategy match {
      case a : DirectLinkStrategy => true
      case b : ValueMatchStrategy if(b.dbCount < thresholdCountValueMatch) => true
      case c : PropertyMatchStrategy if(c.weight > 5) => true
      case _ => false
    }
  }


}
class SimilarityFinder2(qEntity : String)(implicit val knowledgeGraph: KnowledgeGraph) {


  val qEntityGraph = new GraphRDF(qEntity)
  println("In constructor")
  var g : RunnableGraph[Future[Done]] = null

  def createGraph(): Unit = {
    println("create graph called")
//    val finalSink = Sink.combine(printSinkIsSubject, printSinkIsObject)
    val source = getSource(qEntityGraph.statementsList)
    println(s"done getting source..., # statements = ${qEntityGraph.statementsList.size}")
    val sinkPrintTop10 = Sink.foreach((result : List[SimilarEntity]) => Displayer.displayResult(result, 10, qEntity))
    g = RunnableGraph.fromGraph(GraphDSL.create(sinkPrintTop10) {implicit b => sink =>
      val statementGrouper = b.add(unzipStatements)
      val merger = b.add(mergeStrategies)
      val partitioner = b.add(strategyCheapOrExpensivePartitioner(SimilarityFinder2.isACheapStrategy))
      val broadcastSimilarArtists = b.add(Broadcast[List[SimilarEntity]](2))
      val strategyAndGraphRDFZipper = b.add(Zip[Strategy, List[GraphRDF]])
      val similarEntityZipper = b.add(Zip[List[SimilarEntity], List[SimilarEntity]])
      source ~> groupByProperty() ~>statementGrouper.in
      statementGrouper.out0.filter(_._2.nonEmpty) ~> strategyMapperFlow(true) ~> merger.in(0)
      statementGrouper.out1.filter(_._2.nonEmpty) ~> strategyMapperFlow(false) ~> merger.in(1)
      merger.out ~> partitioner.in
      partitioner.out(0) ~> executeCheapStrategies ~> foldFeatureMaps ~> generateSimilarEntities ~> pruneSimilarEntities ~> broadcastSimilarArtists.in
      partitioner.out(1) ~> strategyAndGraphRDFZipper.in0
      broadcastSimilarArtists.out(0) ~> graphRDFMapper ~> strategyAndGraphRDFZipper.in1
      broadcastSimilarArtists.out(1) ~> similarEntityZipper.in0
      strategyAndGraphRDFZipper.out ~> executeExpensiveStrategies ~> foldFeatureMaps ~> generateSimilarEntities.via(Flow[ParIterable[SimilarEntity]].map(_.toList)) ~> similarEntityZipper.in1
      similarEntityZipper.out ~> similarEntityCombiner ~> sink.in
      ClosedShape
    })//.mapMaterializedValue(s => Future{println(s"completed similarity comparison for $qEntity")})
  }
  def runGraph(): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    if(g == null) createGraph()
    val done = g.run()
    Await.result(done, 12 minutes)
  }

  def similarEntityCombiner: Flow[(List[SimilarEntity], List[SimilarEntity]), List[SimilarEntity], NotUsed] = {
    return Flow[Tuple2[List[SimilarEntity], List[SimilarEntity]]]
      .map{case Tuple2(l1: List[SimilarEntity], l2 : List[SimilarEntity]) => {
        l1.map(sEntity => SimilarEntity.combine(sEntity, l2.find(sEntity.name == _.name).getOrElse(throw new Exception("Couldn't find the similar entity"))))
      }}
      .map(_.sorted)
  }

  def executeExpensiveStrategies: Flow[(Strategy, List[GraphRDF]), Map[String, Feature], NotUsed] = {
    return Flow[Tuple2[Strategy,List[GraphRDF]]]
      .map(s => s._1.execute(s._2))
  }

  def graphRDFMapper: Flow[List[SimilarEntity], List[GraphRDF], NotUsed] = {
    return Flow[List[SimilarEntity]]
      .map(s => s.map(sEntity => new GraphRDF(sEntity.name)))
  }

  def pruneSimilarEntities: Flow[ParIterable[SimilarEntity], List[SimilarEntity], NotUsed] = {
    return Flow[ParIterable[SimilarEntity]]
      .map(p => {
        println(s"pruning: the list of similar entities... $p")
        p.toList.sorted.take(SimilarityFinder2.ENTITIES_AFTER_PRUNING)
      })
  }

  def generateSimilarEntities: Flow[ParHashMap[String, ListBuffer[Feature]], ParIterable[SimilarEntity], NotUsed] = {
    return Flow[ ParHashMap[String, ListBuffer[Feature]]]
      .map(hashMap => hashMap.map{case (entity, features) => {
        println(s"creating similar entity: $entity")
        new SimilarEntity(entity, features.toList)
      }})
  }

  def foldFeatureMaps: Flow[Map[String, Feature], ParHashMap[String, ListBuffer[Feature]], NotUsed] = {
    Flow[Map[String, Feature]]
      .fold[mutable.ParHashMap[String, ListBuffer[Feature]]](mutable.ParHashMap[String, ListBuffer[Feature]]())((accumulator, mapOfFeatures) => {
      println("About to fold feature maps...")
      foldMapOfFeatures(accumulator, mapOfFeatures)
    })
  }

  private def foldMapOfFeatures(accumulator: ParHashMap[String, ListBuffer[Feature]], mapOfFeatures: Map[String, Feature]): ParHashMap[String, ListBuffer[Feature]] = {
    addFeatureMaptoAccumulator(mapOfFeatures, accumulator)
    return accumulator
  }

  private def addFeatureMaptoAccumulator(mapOfFeatures: Map[String, Feature], accumulator : ParHashMap[String, ListBuffer[Feature]]) = {
    mapOfFeatures.foreach { case (key, feature) => accumulator.get(key) match {
      case Some(buffer) => {
        buffer.append(feature)
      }
      case None => {
        accumulator.update(key, ListBuffer(feature))
      };
    }
    }
  }

  /*
  import GraphDSL.Implicits._
val foldFlow: Flow[Int, Int, Future[Int]] = Flow.fromGraph(GraphDSL.create(Sink.fold[Int, Int](0)(_ + _)) { implicit builder => fold =>
  FlowShape(fold.in, builder.materializedValue.mapAsync(4)(identity).outlet)
})
  def executeCheapStrategies: Flow[Strategy, Map[String, Feature], NotUsed] = {
    val executionFlow : Flow[Strategy, Map[String, Feature], Future[String]] =
      Flow.fromGraph(GraphDSL.create(Flow[Strategy]
        .map((s) => s.findSimilars())
  })){
   */
  def executeCheapStrategies: Flow[Strategy, Map[String, Feature], NotUsed] = {
    return Flow[Strategy]
      .map((s) => s.findSimilars())
  }

  def strategyCheapOrExpensivePartitioner(isACheapStrategy: (Strategy) => Boolean): Partition[Strategy] = {
    return Partition[Strategy](2, {(s : Strategy) => if(isACheapStrategy(s)) 0 else 1})
  }
  def mergeStrategies = {
    Merge[Strategy](2)
  }

  def unzipStatements = {
    Unzip[Tuple2[String, List[String]], Tuple2[String, List[String]]]
  }


  def getSource(statements: List[(String, String, String)]) : Source[List[(String, String, String)], NotUsed] = {
    return Source.single(statements)
  }
  def strategyMapperFlow(isSubject : Boolean): Flow[Tuple2[String, List[String]], Strategy, NotUsed] = {
    return Flow[Tuple2[String, List[String]]]
      .map{case Tuple2(prop : String, domainOrRange: List[String]) => {
        StrategyFactory.getStrategies(qEntity, qEntityGraph.getTypes, prop, isSubject, domainOrRange)
      }}.mapConcat(s => s)
  }

  def groupByProperty(): Flow[List[(String, String, String)], Tuple2[Tuple2[String, List[String]], Tuple2[String, List[String]]], NotUsed] = {
    return Flow[List[(String, String, String)]]
      .map(statements => statements.groupBy(s => s._2))
      .map(_.map{case (prop, list) =>
        Tuple2(Tuple2(prop, getRange(list)), Tuple2(prop, getDomain(list)))})
      .mapConcat(s =>s)
  }

  private def getDomain(list: List[(String, String, String)]): List[String] = {
    list.filter(_._3 == qEntity).map(_._1)

  }

  private def getRange(list: List[(String, String, String)]): List[String] = {
    list.filter(_._1 == qEntity).map(_._3)
  }
}