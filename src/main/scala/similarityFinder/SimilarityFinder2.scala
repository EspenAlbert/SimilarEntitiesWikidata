package similarityFinder

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.{Flow, GraphDSL, Partition, RunnableGraph, Sink, Source, Unzip, UnzipWith}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF

import scala.collection.immutable.Iterable
import scala.concurrent.Await
import scala.util.{Failure, Success}

import scala.concurrent.duration._
/**
  * Created by espen on 21.03.17.
  */
object SimilarityFinder2 {




}
class SimilarityFinder2(qEntity : String)(implicit val knowledgeGraph: KnowledgeGraph) {
  val qEntityGraph = new GraphRDF(qEntity)
  println("In constructor")
  var g : RunnableGraph[NotUsed] = null

  def createGraph(): Unit = {
    println("create graph called")
    val printSinkIsSubject = Sink.foreach((s:Tuple2[String, List[String]]) => println(s"Subject: $s"))
    val printSinkIsObject = Sink.foreach((s:Tuple2[String, List[String]]) => println(s"Object: $s"))
    val printSink2 = Sink.foreach((s:Tuple2[Tuple2[String, List[String]], Tuple2[String, List[String]]]) => println(s))
    val printSink3 = Sink.foreach((s:List[(String, String, String)]) => println(s))
    val source = getSource(qEntityGraph.getStringIterable)
    println("done getting source...")
    g = RunnableGraph.fromGraph(GraphDSL.create() {implicit b =>
      import GraphDSL.Implicits._
      //      val statementGrouper = b.add(UnzipWith[Tuple2[Tuple2[String, List[String]], Tuple2[String, List[String]]],
      //        Tuple2[String, List[String]],Tuple2[String, List[String]]]
      //        (s => s))
      val statementGrouper = b.add(unzipStatements)

      source ~> groupByProperty() ~>statementGrouper.in
      statementGrouper.out0.filter(_._2.nonEmpty) ~> printSinkIsSubject
      statementGrouper.out1.filter(_._2.nonEmpty) ~> printSinkIsObject
      ClosedShape
    })
  }

  def unzipStatements = {
    Unzip[Tuple2[String, List[String]], Tuple2[String, List[String]]]
  }

  def runGraph(): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    if(g == null) createGraph()
    g.run()
    println("Done running...")
    Thread.sleep(1000)
  }

  def getSource(statements: List[(String, String, String)]) : Source[List[(String, String, String)], NotUsed] = {
    return Source.single(statements)
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
//    println(filtered.length)
//    return filtered

  }

  private def getRange(list: List[(String, String, String)]): List[String] = {
    list.filter(_._1 == qEntity).map(_._3)
  }
}