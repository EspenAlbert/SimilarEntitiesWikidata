package similarityFinder

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink, Source}
import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import core.strategies._
import data.WikidataFactory
import org.scalatest.FunSuite
import similarityFinder.displayer.Displayer
import similarityFinder.ranker.SimilarEntity
import tags.{ActiveSlowTag, ActiveTag}

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParIterable
import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}
import scala.concurrent.duration._
/**
  * Created by espen on 21.03.17.
  */
class TestSimilarityFinder2  extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  import scala.concurrent.ExecutionContext.Implicits.global

  val ringoStarr= WikidataFactory.ringoStarr
  val ringoStarrSimFinder = new SimilarityFinder2(ringoStarr.id)
  test("It should work!!") {
    val simEntities = ringoStarrSimFinder.findSimilarEntities()
    assert(simEntities.size == SimilarityFinder2.ENTITIES_AFTER_PRUNING)
//    Displayer.displayResult(simEntities, 10, ringoStarr.id)
  }
  test("Working for obama?") {
    val obama = WikidataFactory.obama
    val sFinder = new SimilarityFinder2(obama)
    val caught = intercept[AssertionError] {
      sFinder.findSimilarEntities()
    }
    caught match {
        case e: Throwable => println(s"Expected exception...$e"); assert(true)
        case _ => assert(false)
    }
//    val simEntities =
//    assert(simEntities.size == SimilarityFinder2.ENTITIES_AFTER_PRUNING)
//    Displayer.displayResult(simEntities, 2, obama)
  }
  test("group by property flow and unzipper", ActiveTag) {
    val obamaStatements = WikidataFactory.obamaStatements
    val subjectProperties = WikidataFactory.obamaSubjectStatements.map(_._2)
    val objectProperties = WikidataFactory.obamaObjectStatements.map(_._2)
    val sFinder = new SimilarityFinder2(WikidataFactory.obama)
    val bList = ListBuffer[Tuple2[Tuple2[String, List[String]], Tuple2[String, List[String]]]]()
    sFinder.groupByProperty().runWith(Source(List(obamaStatements)), Sink.foreach(a => {
      bList.append(a)
      a match {
        case((prop, listRange), (prop2,listDomain)) => {
          if(subjectProperties.contains(prop)) {
          assert(listRange.nonEmpty)
          assert(listDomain.isEmpty)
        }
          else {
          assert(listRange.isEmpty)
          assert(listDomain.nonEmpty)
          }
        }
      }
    }))//(b => println(b)))

    Thread.sleep(2000)
    val testIsSubjectSink = Sink.foreach((a : Tuple2[String, List[String]]) => assert(subjectProperties.contains(a._1)))
    val testIsObjectSink = Sink.foreach((a : Tuple2[String, List[String]]) => assert(objectProperties.contains(a._1)))
    val g = RunnableGraph.fromGraph(GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._
      val s = Source(bList.toList)
      val statementGrouper = b.add(sFinder.unzipStatements)
      s ~> statementGrouper.in
      statementGrouper.out0.filter(_._2.nonEmpty) ~> testIsSubjectSink
      statementGrouper.out1.filter(_._2.nonEmpty) ~> testIsObjectSink
      ClosedShape
    })

    g.run()
    Thread.sleep(4000)
  }


  test("Strategy mapper flow", ActiveTag) {
    val testSink = Sink.foreach((s: Strategy) => assert(s.isInstanceOf[PropertyMatchStrategy] || s.isInstanceOf[ValueMatchStrategy]))
    val done = ringoStarrSimFinder.strategyMapperFlow(true).runWith(Source(List(Tuple2(ringoStarr.occupationProp, ringoStarr.occupationValues))), testSink)
    Await.result(done._2, 10 seconds)
  }
  test("Strategy splitter", ActiveTag) {
    val expensiveStrategy = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.genderProp, true, ringoStarr.genderValue::Nil).filter(_.isInstanceOf[ValueMatchStrategy])
    val cheapStrategies = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.spouseProp, true, ringoStarr.spouseValues).filter(_.isInstanceOf[DirectLinkStrategy])
    val partitioner = ringoStarrSimFinder.strategyCheapOrExpensivePartitioner(SimilarityFinder2.isACheapStrategy)
    val source = Source[Strategy](List(expensiveStrategy.head) ++ cheapStrategies)
    val assertValueMatchSink = Sink.foreach((s : Strategy) => assert(s.isInstanceOf[ValueMatchStrategy]))
    val assertDirectLinkSink = Sink.foreach((s : Strategy) => assert(s.isInstanceOf[DirectLinkStrategy]))
    val g = RunnableGraph.fromGraph(GraphDSL.create() {implicit b =>
      import GraphDSL.Implicits._
      val partitionerInGraph = b.add(partitioner)
      source ~> partitionerInGraph.in
      partitionerInGraph.out(0) ~> assertDirectLinkSink
      partitionerInGraph.out(1) ~> assertValueMatchSink
      ClosedShape
    })
    val b = g.mapMaterializedValue(s => Future{ println("done") }).run()
    Await.result(b, 10 seconds)
  }
  test("Cheap strategy executor to feature map folder to generate similar entities", ActiveSlowTag) {
    val cheapExecutionFlow = ringoStarrSimFinder.executeCheapStrategies.via(ringoStarrSimFinder.foldFeatureMaps.via(ringoStarrSimFinder.generateSimilarEntities).via(ringoStarrSimFinder.pruneSimilarEntities))
    val strategies = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.occupationProp, true, ringoStarr.occupationValues)
      .filter(_.isInstanceOf[ValueMatchStrategy]).filter(SimilarityFinder2.isACheapStrategy(_))
    println("Cheap strategies: ", strategies)
    assert(strategies.length > 0)
    val runner = cheapExecutionFlow.runWith(
      Source(
        strategies),
      Sink.foreach((similarEntityList : List[SimilarEntity]) => {
        println("Found similar entities: ", similarEntityList.size)
        assert(similarEntityList.size < SimilarityFinder2.ENTITIES_AFTER_PRUNING + 1)
        similarEntityList.foreach((similarEntity) => {
          //        println(s"Sim entity: ${similarEntity.name}")
          assert(similarEntity.sortedFeatures.size > 0)
        })
      }))
    Await.result(runner._2, 3 minutes)
  }
  val beatles = WikidataFactory.theBeatles
  test("expensive strategy executor", ActiveTag) {
    val similarEntities = beatles.members.map(new GraphRDF(_))
    val es1 = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.dateOfBProp, true, ringoStarr.dateOfBValue::Nil)
    val es2 = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.lifestyleProp, true, ringoStarr.lifestyleValue::Nil)
    val es3 = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.genderProp, true, ringoStarr.genderValue::Nil)
    val expensiveStrategies = es1 ++ es2 ++ es3
//    val expensiveStrategies = es2 ++ es3
//          strategyAndGraphRDFZipper.out ~> executeExpensiveStrategies ~> foldFeatureMaps ~> generateSimilarEntities.via(Flow[ParIterable[SimilarEntity]].map(_.toList)) ~> similarEntityZipper.in1

    val cheapStrategyFlow = ringoStarrSimFinder.executeExpensiveStrategies
      .via(ringoStarrSimFinder.foldFeatureMaps)
      .via(ringoStarrSimFinder.generateSimilarEntities)
      .via(Flow[ParIterable[SimilarEntity]].map(_.toList.sorted))
    val result = cheapStrategyFlow.runWith(
      Source(expensiveStrategies.map(s => (similarEntities, s))),
      Sink.foreach(a => {
        a.foreach(se => {
          assert(beatles.members.contains(se.name))
          println(se)
        })
        Displayer.displayResult(a.toList, 4, ringoStarr.id)
      })
    )
    Await.result(result._2, 20 seconds)
  }
  test("coursera play") {

  }
}