package future

import core.globals.KnowledgeGraph
import core.query.specific.QueryFactory
import core.rdf.GraphRDF
import data.WikidataFactory
import org.scalatest.FunSuite

/**
  * Created by espen on 21.03.17.
  */
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

//import scala.concurrent.ExecutionContext.Implicits.global
//object Futures1 {
//  implicit val baseTime = System.currentTimeMillis
//  def main = {
//    def sleep(time: Long) { Thread.sleep(time) }
//
//    // 2 - create a Future
//    val f = Future {
//      sleep(500)
//      1 + 1
//    }
//
//    // 3 - this is blocking (blocking is bad)
//
////    val result = Await.result(f, 1 second)
//    f.onComplete {
//      case Success(value) => println(s"Got the callback @ ${time}, meaning = $value")
//      case Failure(e) => e.printStackTrace
//    }
////    println(s"result was printed @ ${time} $result")
//    println(s"Going to sleep ${time}")
//    sleep(1000)
//
//  }
//  def otherMain = {
//    def getFuture(a : Int): Future[Int] = {
//      Future{
//        Thread.sleep(2000)
//        a
//      }
//    }
//    for(i<- 1 to 5) {
//      println(s"$i")
//      getFuture(i).onComplete{
//        case r => println(s"result: $r")
//      }
//    }
//  }
//
//  private def time() = {
//    System.currentTimeMillis() - baseTime
//  }
//}

class TestFuture extends FunSuite{
//  test("test1 of futures") {
//    val b = Futures1.main
//  }
//  test("otherMain") {
//    Futures1.otherMain
//    Thread.sleep(5000)
//  }
  test("Graph RDF wait for list to be complete") {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val obama = WikidataFactory.obama
    val b = new GraphRDF(obama)
    assert(b.statementsList.length > 200)
  }
  val baseTime = System.currentTimeMillis
  private def time() = {
    System.currentTimeMillis() - baseTime
  }
  test("Playing with query factory") {
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val obama = WikidataFactory.obama

    println(s"About to execute query: ${time()}")
    val b = QueryFactory.findPropertiesAndObjectsFuture(obama)
    b.onComplete{
      case result => println(s"Got result@ ${time()} $result")
    }
    println("About to go to sleep...")
    Thread.sleep(8000)
  }

  test("Getting a future from another file..") {
    import scala.concurrent.ExecutionContext.Implicits.global
    for(i<-1 to 3) {
      println(s"$i")
      FutureReturner.getFuture(i).onComplete {
        r => {
          println(s"received future: $r")
        }
      }
    }
    Thread.sleep(5000)
  }


}
