package core.query

import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */

class MultipleGraphQuery(f : () => String, dataset : String) extends Query(f, dataset) {

   def remove2ndWhere(s : String) : String = {
    val splittedOnWhere = s.split("where")
    if(splittedOnWhere.length < 3) return s
    else {
      return splittedOnWhere(0) + "where " + splittedOnWhere(1) + splittedOnWhere(2)
    }
  }

  override def getQuery(): String = {
    return remove2ndWhere(QueryLocalServer.convertToMultipleGraphQuery(super.getQuery()))
  }
}

