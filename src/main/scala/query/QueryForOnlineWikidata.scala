package query

import java.io.ByteArrayOutputStream

import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */
//TODO: Add order by and LIMIT
class QueryForOnlineWikidata(f : () => String) extends FindSomething{
  val resultStream: ByteArrayOutputStream = new ByteArrayOutputStream
  private var executed = false
  def getResultStream(): ByteArrayOutputStream = {
    return resultStream
  }

  protected def getQuery() : String = {
    return f()
  }
  def executeRaw(): Unit = {
    if(executed) return
    QueryLocalServer.queryOnlineWikidata(f(), resultStream)
    executed = true
  }

  def execute():Unit = {
    if(executed) return
    val first: String = getQuery().replaceFirst("\\{", "where ")
    val wikidataQuery: String = first.substring(0, first.indexOf("UNION"))
    val regexProperties = """entity/P\d{1,5}""".r
    var updatesQuery = wikidataQuery
    for(m <- regexProperties.findAllMatchIn(wikidataQuery)) {
      val matchValue = m.group(0)
      updatesQuery = updatesQuery.replace(matchValue, "prop/direct/" + matchValue.substring(matchValue.indexOf("P")))
    }
    val correctQuery = updatesQuery
    println(correctQuery )
    QueryLocalServer.queryOnlineWikidata(correctQuery, resultStream)
    executed = true
  }

}

