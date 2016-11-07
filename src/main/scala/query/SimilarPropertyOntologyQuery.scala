package query

import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 05.11.2016.
  */
class SimilarPropertyOntologyQuery(query : String) extends Query(query){

  override def execute(): Unit = {
    QueryLocalServer.query(query, resultStream, "similarProperties")
  }

}
