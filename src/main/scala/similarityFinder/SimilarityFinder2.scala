package similarityFinder

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, Partition, RunnableGraph, Sink, Source, Unzip, UnzipWith}
import core.feature.Feature
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF
import core.strategies.{DirectLinkStrategy, Strategy, StrategyFactory, ValueMatchStrategy}

import scala.concurrent.Await
object SimilarityFinder2 {
  val thresholdStrategyWeight = 3.25
  def isACheapStrategy(strategy: Strategy) : Boolean= {
    strategy match {
      case a : DirectLinkStrategy => true
      case b : ValueMatchStrategy if(b.weight > thresholdStrategyWeight) => true
      case _ => false
    }
  }


}
class SimilarityFinder2(qEntity : String)(implicit val knowledgeGraph: KnowledgeGraph) {


  val qEntityGraph = new GraphRDF(qEntity)
  println("In constructor")
  var g : RunnableGraph[NotUsed] = null

  def createGraph(): Unit = {
    println("create graph called")
//    val finalSink = Sink.combine(printSinkIsSubject, printSinkIsObject)
    val source = getSource(qEntityGraph.statementsList)
    println("done getting source...")
    g = RunnableGraph.fromGraph(GraphDSL.create() {implicit b =>
      import GraphDSL.Implicits._
      val statementGrouper = b.add(unzipStatements)
      val merger = b.add(mergeStrategies)
      val partitioner = b.add(strategyCheapOrExpensivePartitioner(SimilarityFinder2.isACheapStrategy))
      source ~> groupByProperty() ~>statementGrouper.in

      statementGrouper.out0.filter(_._2.nonEmpty) ~> strategyMapperFlow(true) ~> merger.in(0)
      statementGrouper.out1.filter(_._2.nonEmpty) ~> strategyMapperFlow(false) ~> merger.in(1)
      merger.out ~> partitioner.in
//      partitioner.out(0)//Cheap strategies
      ClosedShape
    })
  }

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

  def runGraph(): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    if(g == null) createGraph()
    val done = g.run()
//    g.run()(ActorMaterializer.)
//    Await.result(done)
    Thread.sleep(1000)
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