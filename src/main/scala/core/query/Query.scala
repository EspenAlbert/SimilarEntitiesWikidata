package core.query

import java.io.ByteArrayOutputStream
import java.net.SocketTimeoutException

import core.globals.{KnowledgeGraphs, MyDatasets, SimilarPropertyOntology}
import core.query.specific.DatasetInferrer
import core.query.variables.ResultVariable
import jenaQuerier.QueryLocalServer

import scala.util.{Failure, Success, Try}

/**
  * Created by Espen on 02.11.2016.
  */
class Query(f : () => String,  val dataset : String) extends FindSomething{
  private var timedOut = false

  val resultStream: ByteArrayOutputStream = new ByteArrayOutputStream
  private var executed = false
  def getResultStream(): ByteArrayOutputStream = {
    return resultStream
  }

  protected def getQuery() : String = {
    return f()
  }

  override def getResults(variable: String): List[ResultVariable] = {
    if(timedOut) {
//      val prefix = if(dataset.toLowerCase().contains("wikidata")) KnowledgeGraph.getDatasetEntityPrefix(KnowledgeGraph.wikidata) else KnowledgeGraph.getDatasetEntityPrefix(KnowledgeGraph.dbPedia)
//     List(new ResultVariable(prefix + SimilarPropertyOntology.timeoutElement.toString))
      Nil
    }
    else super.getResults(variable)
  }
  def execute():Unit = {
//    println(getQuery())

    if(executed) return
    val execution = Try(
      dataset match {
        case MyDatasets.DsBig => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.DsBig); if (getQuery().contains("similarPropertyOntology")) println("you are probably querying the wrong dataset!!")
        case MyDatasets.SimilarProperties => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.SimilarProperties)
        case MyDatasets.dsWikidata => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.dsWikidata)
        case MyDatasets.`valueMatchWikidata` => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.valueMatchWikidata)
        case MyDatasets.resultsSimilarArtists => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.resultsSimilarArtists)
        case MyDatasets.valueNodeDs => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.valueNodeDs)
        case MyDatasets.`dsDBpedia` => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.dsDBpedia)
        case MyDatasets.strategyMappingWikidata => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.strategyMappingWikidata)
        case MyDatasets.interlinkDBpediaWikidata => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.interlinkDBpediaWikidata)
        case MyDatasets.strategyMappingDBpedia => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.strategyMappingDBpedia)
        case MyDatasets.valueMatchDBpedia => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.valueMatchDBpedia)
        case _ => throw new Exception("Invalid dataset! " + dataset)
      })
    execution match {
      case Success(s) => Unit
      case Failure(e) => {
        if(true) {
          println(s"Exception $e")
          println(s"Timeout for: ${f()}")
        }
        timedOut = true
        Query.hadTimeout = true
      }
    }
      executed = true
  }

}

object Query {
  var hadTimeout : Boolean = false

}

