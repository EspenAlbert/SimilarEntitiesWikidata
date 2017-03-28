package core.interlinking

import core.query.specific.QueryFactory

import scala.util.Try

/**
  * Created by espen on 28.03.17.
  */
object Interlink {
  def fromDBpediaToWikidata(dbPediaId : String) : Try[String] = {
    QueryFactory.findIdWikidataFromDBpedia(dbPediaId)
  }

}
