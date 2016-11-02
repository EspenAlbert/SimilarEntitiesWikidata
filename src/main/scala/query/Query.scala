package query

import java.io.ByteArrayOutputStream

import query.jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */
import Query.multipleGraphQuery
abstract class Query(val query: String) {
  val resultStream: ByteArrayOutputStream = new ByteArrayOutputStream

  def getResultStream(): ByteArrayOutputStream = {
    return resultStream
  }

  def execute():Unit = {
    if(multipleGraphQuery) QueryLocalServer.query(QueryLocalServer.convertToMultipleGraphQuery(query), resultStream)
    else QueryLocalServer.query(query, resultStream)
  }

}

object Query {
  val multipleGraphQuery = true
}
