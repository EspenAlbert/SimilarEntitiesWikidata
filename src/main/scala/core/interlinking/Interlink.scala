package core.interlinking

import core.query.specific.QueryFactory

import scala.util.{Failure, Success, Try}

/**
  * Created by espen on 28.03.17.
  */
object Interlink {
  def fromDBpediaToWikidata(dbPediaId : String) : String = {
    QueryFactory.findIdWikidataFromDBpedia(dbPediaId) match {
      case Success(wikidataId) => wikidataId
      case _ => println(s"Couldn't find wikidata id for $dbPediaId"); dbPediaId
    }
  }
  def fromWikidataToDBpedia(wikidataId : String) : String = {
    QueryFactory.findIdDBpediaFromWikidataId(wikidataId) match {
      case Success(dbPediaId) => dbPediaId
      case _ => println(s"Couldn't find dbpedia id for $wikidataId"); wikidataId
    }
  }

}
