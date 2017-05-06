package core.query

/**
  * Created by espen on 02.05.17.
  */

import core.query.variables.JenaQueryVars.QueryVar
import org.apache.jena.query.{Query, _}

import scala.util.{Failure, Success, Try}

object QueryServerScala {
  def query(ds: String, queryString: String, queryVars: QueryVar*): Unit = {
    val qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + ds + "/query", queryString)
    try {
      val results = qexec.execSelect
      while(results.hasNext){
        val next = results.next()
        queryVars.foreach(_.addResult(next))
      }
    }
    finally qexec.close()
  }
  def decodeResultsQuerySolution(rs : QuerySolution) : (String, String) = {
    (rs.getResource("s").getURI,rs.getResource("s").getURI)
  }

  def getUriFromResult(resultVar : String)(qs : QuerySolution) : String = {
    return qs.getResource(resultVar).getURI
  }
  def getLiteralIntFromResult(resultVar : String)(qs : QuerySolution) : Int = {
    Try(qs.getLiteral(resultVar).getInt) match {
      case Success(s) => s
      case Failure(_) => throw new Exception(s"Failed to decode var: ?$resultVar from query, $qs")
    }
  }

  def getResultsUsingFunctions(results: ResultSet, resultVars: Seq[(QuerySolution) => Any]): List[Iterable[Any]] = {
    if(results.hasNext) {
      val nextResult = results.next()
      resultVars.map(_(nextResult)) :: getResultsUsingFunctions(results, resultVars)
  } else {
    Nil
    }
  }

}
