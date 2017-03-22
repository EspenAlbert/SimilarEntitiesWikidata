package core.query

import java.io.ByteArrayOutputStream

import core.globals.MyDatasets
import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */
class Query(f : () => String,  val dataset : String) extends FindSomething{
  val resultStream: ByteArrayOutputStream = new ByteArrayOutputStream
  private var executed = false
  def getResultStream(): ByteArrayOutputStream = {
    return resultStream
  }

  protected def getQuery() : String = {
    return f()
  }

  def execute():Unit = {
//    println(getQuery())
    if(executed) return
    dataset match {
      case MyDatasets.DsBig => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.DsBig); if(getQuery().contains("similarPropertyOntology")) println("you are probably querying the wrong dataset!!")
      case MyDatasets.SimilarProperties => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.SimilarProperties)
      case MyDatasets.ValueMatch => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.ValueMatch)
      case MyDatasets.ResultsSimilarArtists => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.ResultsSimilarArtists)
      case MyDatasets.valueNodeDs => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.valueNodeDs)
      case MyDatasets.DBpediaDS => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.DBpediaDS)
      case MyDatasets.strategyMappingWikidata => QueryLocalServer.query(getQuery(), resultStream, MyDatasets.strategyMappingWikidata)
      case _ => throw new Exception("Invalid dataset! " + dataset)
    }
    executed = true
  }

}

