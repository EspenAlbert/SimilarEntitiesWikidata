package query

import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 08.11.2016.
  */
object AskQuery {
  def ask(f : () => String, dataset : String = "ds") : Boolean = {
    return QueryLocalServer.ask(QueryLocalServer.convertToMutlipleGraphQueryWithoutSelect(f()), dataset)
  }


}
