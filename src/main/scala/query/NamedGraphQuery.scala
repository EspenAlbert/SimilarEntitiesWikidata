package query

/**
  * Created by Espen on 02.11.2016.
  */
import NamedGraphQuery.getQuery
abstract class NamedGraphQuery(query: String, graph:String) extends Query(getQuery(query, graph)){


}

object NamedGraphQuery {
  def getQuery(query:String, namedGraph:String): String = {
    val fromLine = s"FROM <$namedGraph>\n"
    val indexOfSelect = query.indexOf("select")
    val indexOfSelectEndOfLIne = query.indexOf("\n", indexOfSelect)
    return query.substring(0, indexOfSelectEndOfLIne + 1) + fromLine + query.substring(indexOfSelectEndOfLIne + 1)
  }
}


