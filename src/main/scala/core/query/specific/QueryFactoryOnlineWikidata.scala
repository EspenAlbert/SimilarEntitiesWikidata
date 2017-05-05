package core.query.specific

import core.query.QueryForOnlineWikidata

import scala.util.Try

/**
  * Created by espen on 05.05.17.
  */
object QueryFactoryOnlineWikidata {

  def findLabel(entityId: String) : Try[String] = {
    val commonPrefixes = "PREFIX wd: <http://www.wikidata.org/entity/>\nPREFIX wdt: <http://www.wikidata.org/prop/direct/>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
    val labelQuery = new QueryForOnlineWikidata(() => commonPrefixes + s"select ?label \n where { <$entityId> rdfs:label ?label . \n filter(lang(?label) = 'en')\n }")
    labelQuery.executeRaw()
    Try(labelQuery.getResults("label")(0))
  }
}
