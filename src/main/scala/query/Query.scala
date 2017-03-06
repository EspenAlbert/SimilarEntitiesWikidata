package query

import java.io.ByteArrayOutputStream

import globals.MyDatasets
import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */
//TODO: Add order by and LIMIT
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
      case _ => throw new Exception("Invalid dataset! " + dataset)
    }
    executed = true
  }

}

