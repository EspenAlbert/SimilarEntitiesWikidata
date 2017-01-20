package query

/**
  * Created by Espen on 02.11.2016.
  */

object NamedGraphQuery {
  def getFrom(namedGraph:String): String = {
    return s"FROM <$namedGraph>\n"
  }
}


