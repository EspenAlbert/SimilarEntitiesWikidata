package similarityFinder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source}
import core.globals.KnowledgeGraph
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.ActiveTag

import scala.collection.mutable.ListBuffer

/**
  * Created by espen on 21.03.17.
  */
class TestSimilarityFinder2  extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("It should work!!") {
    val obama = WikidataFactory.obama
    val a = new SimilarityFinder2(obama)
    a.runGraph()
  }
  test("group by property flow and unzipper", ActiveTag) {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
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
      println(a)
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
    Thread.sleep(2000)
  }
  test("unzipper", ActiveTag) {
    val initial = "((http://www.wikidata.org/entity/P180,List()),(http://www.wikidata.org/entity/P180,List(http://www.wikidata.org/entity/Q2915674,  http://www.wikidata.org/entity/Q4858105,  http://www.wikidata.org/entity/Q5842038)))\n((http://www.wikidata.org/entity/P106,List(http://www.wikidata.org/entity/Q82955,  http://www.wikidata.org/entity/Q40348,  http://www.wikidata.org/entity/Q15958642)),(http://www.wikidata.org/entity/P106,List()))"
  }

}
