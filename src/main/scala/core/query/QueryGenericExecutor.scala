package core.query

import core.query.variables.ResultVariable

/**
  * Created by espen on 02.05.17.
  */
object QueryGenericExecutor {

  def executeQuery(queryString : String, resultVars : Iterable[String], dataset : String) : Iterable[List[ResultVariable]] = {
    val query = new Query(() => queryString, dataset)
    query.execute()
    return resultVars.map(query.getResults(_))
  }

}
