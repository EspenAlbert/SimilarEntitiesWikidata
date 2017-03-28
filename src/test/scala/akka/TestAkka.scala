package akka

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.impl.FanOut
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, MergePreferred, Partition, RunnableGraph, Sink, Source}
import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
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
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  test("Akka materializing") {
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

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => xs.takeWhile(_ == x) :: pack(xs.dropWhile(_ == x))
  }

  def encode[T](strings: List[T]): List[(T, Int)] = {
    val packed = pack(strings)
    return packed.map(l => (l.head, l.length))
  }
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    xs.foldRight(List[U]())( f(_) :: _  )

  def lengthFun[T](xs: List[T]): Int = {
    xs.foldRight(0)((a,b) => 1 + b)

  }
  test("Coursera play") {
    val actual = pack(List("a", "a", "a", "b", "c", "c", "a"))
    val expected = List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
    assert(actual == expected)
    val encodeActual = encode(List("a", "a", "a", "b", "c", "c", "a"))
    val encodeExpected = List(("a", 3), ("b", 1), ("c", 2), ("a", 1))
    assert(encodeActual == encodeExpected)
    println(1 :: List())
  }
  test("akka merge preferred") {
    val source = Source.fromIterator(() => Iterator.from(1))
    def getIterator = Iterator.from(1)
    val i = getIterator
    println(i.next(), i.next(), i.next())
    val sink = Sink.foreach((a : Int) => println(s"Sink received : $a"))
    val g = RunnableGraph.fromGraph(GraphDSL.create(sink) { implicit b => sink =>
      import GraphDSL.Implicits._

      val merge = b.add(MergePreferred[Int](1))
      val bcast = b.add(Broadcast[Int](2))

      source ~> merge.preferred
      merge ~> Flow[Int].map { s => println(s); s } ~> bcast ~> sink.in
      merge           <~ Flow[Int].filter(_ > 5).buffer(200, OverflowStrategy.backpressure) <~                            bcast
      ClosedShape
    })
    val done = g.run()
    Await.result(done, 5.seconds)
  }
  test("identity on mapConcat") {
    import GraphDSL.Implicits._
    val s = Source(List(List("a"), List("bc"), List("def")))
    val p21 = s.watchTermination()((t, ab) => ab.onComplete{case Success(s51) => println("SourceFinished")})
    val sink: Sink[String, Future[Done]] = Sink.foreach((c: String) => println(c + "\n"))
    val concater = Flow[List[String]].mapConcat(identity)
    val runnable = s.via(concater).toMat(sink)(Keep.right)
    val identityStringFlow = Flow[String].map(identity)
    val stringCounterInt: Sink[String, Future[Int]] = Flow[String].map(s => 1).toMat(Sink.fold(0)((a, b) => a + b))(Keep.right)
    val a = Sink.fold(ListBuffer[String]())((l: ListBuffer[String], e: String) => {
      l.+=:(e)
      l
    })
    val r3 = s.via(concater).toMat(a)(Keep.right)
    val d2 = r3.run()
    val res1 = Await.result(d2, 2.seconds)
    val s2 = Source.fromIterator(() => res1.iterator)
    val r4 = s2.runWith(sink)
    val futureDone = Await.result(r4, 2.seconds)
    println(s"Done... $futureDone")

    //    RunnableGraph.fromGraph(GraphDSL.create(
    //    val splitter = Broadcast[String](2).
    val stringCounter = Flow.fromGraph(GraphDSL.create(identityStringFlow, stringCounterInt, stringCounterInt)((_, _, _)) { implicit builder =>
      (sFlow, iFlow, iFlow2) =>
//        Source.single("a") ~> sFlow ~> iFlow
        val broadcaster = builder.add(Broadcast[String](3))
//        broadcaster.out(1).
//        sFlow ~> broadcaster.in
        broadcaster.out(1) ~> sFlow.in
        broadcaster.out(0) ~> iFlow.in
        broadcaster.out(2) ~> iFlow2.in
//        FlowShape(broadcaster.in, sFlow.out,builder.materializedValue.mapAsync(2)((k) => k._2).outlet)
        FlowShape(broadcaster.in, sFlow.out)
//      FanOutShape2(sFlow.in,sFlow.out, builder.materializedValue.mapAsync(2)((k) => k._2).outlet)
    })

    val graphWithC = s.via(concater).viaMat(stringCounter)(Keep.right).toMat(Sink.foreach((l: String)=> println(s"Received # $l")))(Keep.both)
    val run1 = graphWithC.run()
    run1._2.onComplete{
      case Success(s) => println(s"Materializer of full graph complete $s")
    }
    run1._1._2.onComplete{
      case Success(s) => println(s"Materializer inside of graph complete $s")
    }
    run1._1._3.onComplete{
      case Success(s) => println(s"Materializer inside of graph 2 complete $s")
    }
//    Await.result(run1._2, 5.seconds)
    println("..........DONE..........")
//    st

    //    val flowWithExtraMaterializer : Flow[String, String, Future[String]]= Flow[String].viaMat(_ + "1")(List[String]())(.map((s: String) => s + "a")((f) => f)
    val stringCollector: Flow[String, String, Future[String]] = Flow.fromGraph(GraphDSL.create(Sink.fold[String, String]("Start|")(_ + _)) { implicit builder =>
      sCollector =>
        FlowShape(sCollector.in, builder.materializedValue.mapAsync(2)(identity).outlet)
    })
    val foldFlow: Flow[Int, Int, Future[Int]] = Flow.fromGraph(GraphDSL.create(Sink.fold[Int, Int](0)(_ + _)) { implicit builder =>
      fold =>
        FlowShape(fold.in, builder.materializedValue.mapAsync(4)(identity).outlet)
    })
    val done = runnable.run()
    Await.result(done, 2.seconds)
    val r2 = s.via(concater).viaMat(stringCollector)(Keep.right).toMat(sink)(Keep.both)
    val (fString, fDone) = r2.run()
    val sResult = Await.result(fString, 10.seconds)
    println(sResult)
    val d = Await.result(fDone, 2.seconds)
    d match {
      case Done => println("Finished with success")
    }

  }
  test("another example" ) {
    import GraphDSL.Implicits._
//    val a = Flow[Int].viaMat(Flow[Int](_ +2))((a, b) => b)
//    val a2 = Flow[Int].viaMat(Flow[Int].map(_ + 3))(Keep.right)
//    val s = Source(List(1,2,3))
//    def receivier(a : Int): Unit = {
//      println(s"Receiver: $a")
//    }
//    val runner = s.viaMat(a)(receivier).via(a2).runWith(Sink.foreach(println(_)))
    val wordsRaw = List("w1", "w2", "w1", "w2", "W", "W", "W")
    val MaximumDistinctWords = 50
    val words = Source(wordsRaw)
    val counts: Source[(String, Int), NotUsed] = words
      // split the words into separate streams first
      .groupBy(MaximumDistinctWords, identity)
      //transform each element to pair with number of words in it
      .map(_ -> 1)
      // add counting logic to the streams
      .reduce((l, r) => (l._1, l._2 + r._2))
    .mergeSubstreams
      // get a stream of word counts
    val done = counts.runWith(Sink.foreach(a => println(a)))
    Await.result(done, 3.seconds)
    def reduceByKey[In, K, Out](
                                 maximumGroupSize: Int,
                                 groupKey:         (In) => K,
                                 map:              (In) => Out)(reduce: (Out, Out) => Out): Flow[In, (K, Out), NotUsed] = {

      Flow[In]
        .groupBy[K](maximumGroupSize, groupKey)
        .map(e => groupKey(e) -> map(e))
        .reduce((l, r) => l._1 -> reduce(l._2, r._2))
        .mergeSubstreams
    }

    val wordCounts = words.via(
      reduceByKey(
        MaximumDistinctWords,
        groupKey = (word: String) => word,
        map = (word: String) => 1)((left: Int, right: Int) => left + right))

    val done2 = wordCounts.runWith(Sink.foreach(a => println(a)))
    Await.result(done2, 5.seconds)

  }
  test("multiple argumetns to builder") {
    import GraphDSL.Implicits._
    val topHeadSink = Sink.head[Int]
    val bottomHeadSink = Sink.head[Int]
    val sharedDoubler = Flow[Int].map(_ * 2)

    val r = RunnableGraph.fromGraph(GraphDSL.create(topHeadSink, bottomHeadSink)((_, _)) { implicit builder =>
      (topHS, bottomHS) =>
        import GraphDSL.Implicits._
        val broadcast = builder.add(Broadcast[Int](2))
        Source.single(1) ~> broadcast.in

        broadcast.out(0) ~> sharedDoubler ~> topHS.in
        broadcast.out(1) ~> sharedDoubler ~> bottomHS.in
        ClosedShape
    })
  }

}
