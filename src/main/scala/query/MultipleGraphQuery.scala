package query

import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */

abstract class MultipleGraphQuery(query: String) extends Query(query){

  override def execute():Unit = {
    QueryLocalServer.query(QueryLocalServer.convertToMultipleGraphQuery(query), resultStream)
  }

}

object MultipleGraphQuery {
}


