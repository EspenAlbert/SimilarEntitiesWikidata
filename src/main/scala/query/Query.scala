package query

import java.io.ByteArrayOutputStream

import query.jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */
abstract class Query(val query: String) {
  val resultStream: ByteArrayOutputStream = new ByteArrayOutputStream

  def getResultStream(): ByteArrayOutputStream = {
    return resultStream
  }

  def execute():Unit = {
    QueryLocalServer.query(query, resultStream)
  }

}

object Query {
}
