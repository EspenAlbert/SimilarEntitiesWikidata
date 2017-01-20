package query

import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */

class MultipleGraphQuery(f : () => String, dataset : String) extends Query(f, dataset) {

  override def getQuery(): String = {
    return QueryLocalServer.convertToMultipleGraphQuery(super.getQuery())
  }
}

