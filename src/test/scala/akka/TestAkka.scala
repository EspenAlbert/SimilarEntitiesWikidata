package akka

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape, FlowShape}
import akka.stream.scaladsl.{Flow, GraphDSL, Partition, RunnableGraph, Sink, Source}
import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import org.scalatest.FunSuite

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
/**
  * Created by espen on 21.03.17.
  */
class TestAkka extends FunSuite{
  test("Akka should work") {
    val nestedSource =
      Source.single(0) // An atomic source
        .map(_ + 1) // an atomic processing stage
        .named("nestedSource") // wraps up the current Source and gives it a name

    val nestedFlow =
      Flow[Int].filter(_ != 0) // an atomic processing stage
        .map(_ - 2) // another atomic processing stage
        .named("nestedFlow") // wraps up the Flow, and gives it a name

    val nestedSink =
      nestedFlow.to(Sink.fold(0)(_ + _)) // wire an atomic sink to the nestedFlow
        .named("nestedSink") // wrap it up

    // Create a RunnableGraph
    val runnableGraph = nestedSource.to(nestedSink)
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    runnableGraph.run()
    val printSink = Sink.foreach((s : String) => println(s)).named("printSink")
    val printSink2 = Sink.foreach((s : Int) => println(s))
    val stringConverter = Flow[Int].map(s => s.toString)
    val runnableGraph2 = nestedSource.via(stringConverter).to(Sink.head).run()
    val runnableGraph3 = nestedSource.via(stringConverter).to(printSink).run()
    val runnableGraph4 = nestedSource.to(printSink2).run()

    val listOfStrings = List("a", "b", "c", "d")
    val s1 = Source.fromIterator(() => listOfStrings.iterator)
    val f1 = Flow[String].zipWithIndex
    val f2 = Flow[(String, Long)].map(s => s._1 + s._2)
    val runnableGraph5 = s1.via(f1).via(f2).to(printSink)
    runnableGraph5.run()

    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val obama = "http://www.wikidata.org/entity/Q76"
    val s2 = Source.single(new GraphRDF(obama))
    def getSource(s : String) : Source[GraphRDF, NotUsed] = {
      return Source.single(new GraphRDF(s))
    }
    val f21 = Flow[GraphRDF].map(g => g.statementsList)
    val f22 = Flow[List[(String, String, String)]].mapConcat(s => s)
    val f23 = Flow[(String, String, String)].map(s => List(s._1, s._2, s._3).mkString("-")).filter(_.startsWith("http://www.wikidata.org/entity/Q19"))
    val f24 = Flow[(String, String, String)].map(s => List(s._1, s._2, s._3).mkString("OBAMA"))
//    val runnableGraph6 = s2.via(f21).via(f22).via(f23).via(f1).via(f2).to(printSink).run()

    val partitionFlow = Partition[(String, String, String)](2, (s=> if(s._1 == obama) 0 else 1))

    val g = RunnableGraph.fromGraph(GraphDSL.create() {implicit b =>
      import GraphDSL.Implicits._
      val p = b.add(partitionFlow)

      getSource(obama) ~> f21 ~>f22 ~>p.in
      p.out(0) ~> f24 ~>printSink
      p.out(1) ~> f23 ~> printSink
      ClosedShape
    })
    g.run()


  }
  def mTester(a : Unit, done: Future[Done]): Unit = {
    done.onComplete {
      case Success(c) => println(s"Materializer got success: $c")
      case Failure(c) => println(s"Materializer got failure: $c")
    }
  }
  def mTesterInt(b: NotUsed, a: Future[Int]): Unit = {
    a.onComplete {
      case Success(c) => println(s"Materializer Int got success: $c")
      case Failure(c) => println(s"Materializer Int got failure: $c")
    }
  }
  case class MyClass(val a : Future[Int]) {
    def close() = a.onComplete {
      case Success(c) => println(s"Materializer Int got success: $c")
      case Failure(c) => println(s"Materializer Int got failure: $c")
    }
  }
  test("Akka materializing") {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    import GraphDSL.Implicits._
    val foldFlow: Flow[Int, Int, Future[Int]] = Flow.fromGraph(GraphDSL.create(Sink.fold[Int, Int](0)(_ + _)) { implicit builder => fold =>
      FlowShape(fold.in, builder.materializedValue.mapAsync(4)(identity).outlet)
    })
//    val sink : Sink[Int, Future[Done]]= Sink.foreach((a : Int) => println(a))
    val sink = Sink.foreach((a : Int) => println(a))
//    val runnable : RunnableGraph[Future[Done]]= Source(1 to 10).viaMat(foldFlow)(mTesterInt).via(Flow[Int].map(a => {
//      Thread.sleep(3000)
//      a
//    })).to(sink)//.mapMaterializedValue(a => Future{true})//(mTester)
    val runnable= Source(1 to 10).via(foldFlow).via(Flow[Int].map(a => {
        Thread.sleep(3000)
        a
      })).to(sink)//.mapMaterializedValue(a => Future{true})//(mTester)
    val r2 = Source(1 to 10).to(sink)
    val done = runnable.run()
    Thread.sleep(5000)
//    Await.result(done, 5 seconds)

  }

}
