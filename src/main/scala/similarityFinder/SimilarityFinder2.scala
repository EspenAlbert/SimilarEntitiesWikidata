package similarityFinder

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Merge, Partition, RunnableGraph, Sink, Source, Unzip, UnzipWith, Zip}
import core.feature.Feature
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.{GraphRDF, GraphRDFDescriptivePropertyChecker}
import core.strategies._
import shapeless.ops.hlist.ZipWith
import similarityFinder.displayer.Displayer
import similarityFinder.ranker.SimilarEntity

import scala.concurrent.duration._
import scala.collection.parallel.mutable
import scala.collection.parallel.mutable.{ParHashMap, ParIterable}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import GraphDSL.Implicits._
import core.globals.SimilarPropertyOntology

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.immutable.ParMap
object SimilarityFinder2 {
  val thresholdStrategyWeight = 3.25
  val ENTITIES_AFTER_PRUNING = 1000
  def isACheapStrategy(strategy: Strategy) : Boolean= {
    strategy match {
      case a : DirectLinkStrategy => true
      case b : ValueMatchStrategy if(b.dbCount < MyConfiguration.thresholdCountCheapStrategy && b.property != SimilarPropertyOntology.wikiPageWikiLink.toString) => true
      case c : PropertyMatchStrategy if(c.dbCount < MyConfiguration.thresholdCountCheapStrategy) => true
      case a @ (_:SearchUndirectedL1Strategy | _:SearchDirectedL1Strategy| _: SearchDirectedL2Strategy | _:SearchUndirectedL2Strategy) => true
      case a : AggregatorStrategy => true
      case a : ExpandNodeStrategy => true
      case _ => false
    }
  }
}
class SimilarityFinder2(qEntity : String,systemParam: ActorSystem = null, materializerParam: ActorMaterializer = null, useFilteringGraphRDF: Boolean = false)(implicit val knowledgeGraph: KnowledgeGraph) {


  implicit val system = if(systemParam == null) ActorSystem("my-system") else systemParam
  implicit val materializer = if(materializerParam == null) ActorMaterializer(ActorMaterializerSettings(system).withInputBuffer(2048, 2048)) else materializerParam

  val qEntityGraph = if(useFilteringGraphRDF) new GraphRDFDescriptivePropertyChecker(qEntity) else new GraphRDF(qEntity)
  var expensiveStrategyList : Seq[Strategy] = null

  def findInitialEntitiesAsSet() : Set[String] = {
    val (cheapStrategies, _) = getStrategiesCheapAndExpensive()
    val execution = executeCheapStrategiesGraphNoFeatures(cheapStrategies).run()
    return Await.result(execution, 15 minutes)
  }
  def findInitialEntitiesAsMap() : ParHashMap[String, ListBuffer[Feature]] = {
    val (cheapStrategies, _) = getStrategiesCheapAndExpensive()
    val execution = executeCheapStrategiesGetFeatureMaps(cheapStrategies).run()
    return Await.result(execution, 15 minutes).head
  }

  def findInitialEntities(): List[SimilarEntity] = {
    val (cheapStrategies, expensiveStrategies) = getStrategiesCheapAndExpensive()
    expensiveStrategyList = expensiveStrategies
    println(s"$knowledgeGraph Cheap strategies : ${cheapStrategies.size} = $cheapStrategies")
    println(s"$knowledgeGraph Expensive strategies : ${expensiveStrategies.size} = $expensiveStrategies")
    val cheapExecution = executeCheapStrategiesGraph(cheapStrategies, true)
    val initialSimilarEntities = Await.result(cheapExecution.run(), 10 minutes)
    println(s"Initial similar entities found in KG: $knowledgeGraph was ${initialSimilarEntities.size}")
    return initialSimilarEntities
  }
  def findSimilarToRestOfStrategies(entities: List[SimilarEntity]): List[SimilarEntity] = {
    return completeComparison(expensiveStrategyList, entities)
  }

  def findSimilarEntities(): List[SimilarEntity] = {
    val initialSimilarEntities = findInitialEntities()
    return completeComparison(expensiveStrategyList, initialSimilarEntities)
  }

  private def completeComparison(strategies: Seq[Strategy], similarEntities: List[SimilarEntity]): List[SimilarEntity] = {
    val similarEntitiesExpensiveGraph = executeExpensiveStrategiesGraph(strategies, similarEntities)
    val similarEntitiesExpensiveExecution = Await.result(similarEntitiesExpensiveGraph.run(), 10 minutes)
//    assert(similarEntities.size == similarEntitiesExpensiveExecution.size)NOT VALID ANYMORE, E.G. ENTITY FROM OTHER DATASET WILL NOT BE REPRESENTED...
    val resultCombinerGraph = combineSimilarEntitiesGraph(similarEntitiesExpensiveExecution, similarEntities)
    val finalResult = Await.result(resultCombinerGraph.run(), 1 minute)
    return finalResult
  }

  def produceStrategies(): RunnableGraph[(Future[ListBuffer[Strategy]], Future[ListBuffer[Strategy]])] = {
    val source = getSource(qEntityGraph.statementsList)
    def strategyFolder = Sink.fold(ListBuffer[Strategy]())((l : ListBuffer[Strategy], s: Strategy) => {
      l.append(s)
      l
    })
    return RunnableGraph.fromGraph(GraphDSL.create(strategyFolder, strategyFolder)((_,_)){implicit b =>
      (cheapStrategiesSink, expensiveStrategiesSink) => {

        val statementGrouper = b.add(unzipStatements)
        val merger = b.add(mergeStrategies)
        val partitioner = b.add(strategyCheapOrExpensivePartitioner(SimilarityFinder2.isACheapStrategy))

        source ~> groupByProperty() ~>statementGrouper.in
        statementGrouper.out0.filter(_._2.nonEmpty) ~> strategyMapperFlow(true) ~> merger.in(0)
        statementGrouper.out1.filter(_._2.nonEmpty) ~> strategyMapperFlow(false) ~> merger.in(1)
        merger.out ~> partitioner.in
        partitioner.out(0) ~> cheapStrategiesSink.in
        partitioner.out(1) ~> expensiveStrategiesSink.in
        ClosedShape
      }
    })
  }
  def foldSimilarEntities : Sink[List[SimilarEntity], Future[List[SimilarEntity]]] = Sink.fold(List[SimilarEntity]())(_ ++ _)

  def executeCheapStrategiesGraph(cheapStrategies : Seq[Strategy], awaitPruning : Boolean= false): RunnableGraph[Future[List[SimilarEntity]]] = {
    val source: Source[Strategy, NotUsed] = sourceStrategies(cheapStrategies)
    return RunnableGraph.fromGraph(GraphDSL.create(foldSimilarEntities){implicit b =>
      sink =>
      if(awaitPruning) {
        source ~> executeCheapStrategies ~> foldFeatureMaps ~> generateSimilarEntities.map(_.toList) ~> sink.in

      }
      else {
        source ~> executeCheapStrategies ~> foldFeatureMaps ~> generateSimilarEntities ~> pruneSimilarEntities ~> sink.in

      }
      ClosedShape
    })
  }
  def executeCheapStrategiesGetFeatureMaps(cheapStrategies : Seq[Strategy], awaitPruning : Boolean= false): RunnableGraph[Future[immutable.Seq[ParHashMap[String, ListBuffer[Feature]]]]] = {
    val source: Source[Strategy, NotUsed] = sourceStrategies(cheapStrategies)
    val seqSink = Sink.seq[ParHashMap[String, ListBuffer[Feature]]]
//    val sink : Sink[Map[String, ListBuffer[Feature]], Future[Map[String, ListBuffer[Feature]]]]= Sink.fold(Map[String, ListBuffer[Feature]])((acc, nextmap) => acc ++ nextmap)
    return RunnableGraph.fromGraph(GraphDSL.create(seqSink){implicit b =>
      sink =>
        source ~> executeCheapStrategies ~> foldFeatureMaps ~>  sink.in
      ClosedShape
    })
  }
  def foldSimilarEntitiesSet : Sink[Map[String, Feature], Future[Set[String]]] = Sink.fold[Set[String], Map[String, Feature]](Set())((acc, nextMap) => acc ++ nextMap.keySet)


  def executeCheapStrategiesGraphNoFeatures(cheapStrategies : Seq[Strategy]): RunnableGraph[Future[Set[String]]] = {
    val source: Source[Strategy, NotUsed] = sourceStrategies(cheapStrategies)
    return RunnableGraph.fromGraph(GraphDSL.create(foldSimilarEntitiesSet){implicit b =>
      sink =>
        source ~> executeCheapStrategies ~> sink.in
        ClosedShape

    })
  }
  private def sourceStrategies(strategies : Seq[Strategy]): Source[Strategy, NotUsed] = {
    return Source.fromIterator(() => strategies.iterator)
  }

  def graphRdfCreator(similarEntities: List[SimilarEntity]) : Future[List[GraphRDF]] ={
    val graphSink = Sink.fold(List[GraphRDF]())((l: List[GraphRDF], gRdf: List[GraphRDF]) => {
      l ++ gRdf
    })
    return Source(List(similarEntities)).via(graphRDFMapper).runWith(graphSink)

  }

  def executeExpensiveStrategiesGraph(expensiveStrategies: Seq[Strategy], similarEntities: List[SimilarEntity]): RunnableGraph[Future[List[SimilarEntity]]] ={
    val s = Await.result(graphRdfCreator(similarEntities), 2 minutes)
    println("Created graph rdfs for the 1k entities")
    return RunnableGraph.fromGraph(GraphDSL.create(foldSimilarEntities){implicit b =>
      sink =>
        val strategyAndGraphRDFZipper = b.add(Zip[List[GraphRDF], Strategy])

        val strategySource = sourceStrategies(expensiveStrategies)
        val graphRdfSource = Source.repeat(s)
        strategySource ~> strategyAndGraphRDFZipper.in1
        graphRdfSource~> strategyAndGraphRDFZipper.in0
        strategyAndGraphRDFZipper.out ~> executeExpensiveStrategies ~> foldFeatureMaps ~> generateSimilarEntities.via(Flow[ParIterable[SimilarEntity]].map(_.toList)) ~> sink.in
        ClosedShape
    })
  }
  def combineSimilarEntitiesGraph(similarEntities : List[SimilarEntity],similarEntities2 : List[SimilarEntity]): RunnableGraph[Future[List[SimilarEntity]]] = {
    return RunnableGraph.fromGraph(GraphDSL.create(foldSimilarEntities){implicit b =>
      sink =>
        val similarEntityZipper = b.add(Zip[List[SimilarEntity], List[SimilarEntity]])
        val s1 = Source(List(similarEntities))
        val s2 = Source(List(similarEntities2))
        s1 ~> similarEntityZipper.in0
        s2 ~> similarEntityZipper.in1
        similarEntityZipper.out ~> similarEntityCombiner~> sink.in
        ClosedShape
    })  }

  private def getStrategiesCheapAndExpensive() : (Seq[Strategy], Seq[Strategy]) = {
    val strategyProducer = produceStrategies()
    val (cheapStrategiesFuture, expensiveStrategiesFuture) = strategyProducer.run()
    val cheapStrategies = Await.result(cheapStrategiesFuture, 12 minutes)
    val expensiveStrategies = Await.result(expensiveStrategiesFuture, 12 minutes)
    return (cheapStrategies, expensiveStrategies)
  }
  def findSimilarityTo(similarEntities: List[String]): List[SimilarEntity] = {
    val (cheapStrategies, expensiveStrategies) = getStrategiesCheapAndExpensive()
    val similarEntitiesExpensiveGraph = executeExpensiveStrategiesGraph(expensiveStrategies++cheapStrategies, similarEntities.map(new SimilarEntity(_,Nil)))
    val similarEntitiesExpensiveExecution = Await.result(similarEntitiesExpensiveGraph.run(), 5 minutes)
    try {
      assert(similarEntities.size == similarEntitiesExpensiveExecution.size)
    } catch {
      case a : AssertionError => println("Some similar entity was not found.../had no statements")
    }
    return similarEntitiesExpensiveExecution
  }

  def similarEntityCombiner: Flow[(List[SimilarEntity], List[SimilarEntity]), List[SimilarEntity], NotUsed] = {
    return Flow[Tuple2[List[SimilarEntity], List[SimilarEntity]]]
      .map{case Tuple2(l1: List[SimilarEntity], l2 : List[SimilarEntity]) => {
        l1.map(sEntity => SimilarEntity.combine(sEntity, l2.find(sEntity.name == _.name).getOrElse(throw new Exception("Couldn't find the similar entity"))))
      }}
      .map(_.sorted)
  }

  def executeExpensiveStrategies: Flow[(List[GraphRDF],Strategy), Map[String, Feature], NotUsed] = {
    return Flow[Tuple2[List[GraphRDF], Strategy]]
      .map(s => s._2.execute(s._1))
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
        new SimilarEntity(entity, features.toList)
      }})
  }

  def foldFeatureMaps: Flow[Map[String, Feature], ParHashMap[String, ListBuffer[Feature]], NotUsed] = {
    Flow[Map[String, Feature]]
      .fold[mutable.ParHashMap[String, ListBuffer[Feature]]](mutable.ParHashMap[String, ListBuffer[Feature]]())((accumulator, mapOfFeatures) => {
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
  def executeCheapStrategies: Flow[Strategy, Map[String, Feature], NotUsed] = {
    return Flow[Strategy]
      .mapAsyncUnordered(4)((s) => Future {s.findSimilars()})
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
      .mapAsyncUnordered(4){case Tuple2(prop : String, domainOrRange: List[String]) => {
        Future{StrategyFactory.getStrategies(qEntity, qEntityGraph.getTypes, prop, isSubject, domainOrRange)}
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