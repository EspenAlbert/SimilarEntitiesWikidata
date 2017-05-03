package core.query

/**
  * Created by espen on 02.05.17.
  */

import core.globals.MyDatasets
import core.query.specific.QueryFactoryJena
import org.apache.jena.query.{Query, _}
import org.apache.jena.rdf.model.Resource

import scala.util.{Failure, Success, Try}

object QueryServerScala {
  def query(ds: String, queryString: String, queryVars: QueryFactoryJena.QueryVar*): Unit = {
    val query = QueryFactory.create(queryString)
    val qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + MyDatasets.dsWikidata + "/query", queryString)
    try {
      val results = qexec.execSelect
      while(results.hasNext){
        val next = results.next()
        queryVars.foreach(_.addResult(next))
      }
    }
    finally qexec.close()
  }

  def main(args: Array[String]): Unit = {
//    val returnedStream = queryTest(decodeResultsQuerySolution)
//    println(returnedStream)
//    println(returnedStream.take(50))
    val myFunctionOnS = getUriFromResult("s")(_)
    val myFunctionOnS2 = getUriFromResult("s")(_)
    val myFunctionOnS3 = getLiteralIntFromResult("b")(_)
    val theList = List(myFunctionOnS, myFunctionOnS2, myFunctionOnS3)
    val returnedIterable = queryTest(theList:_*)
    for {
      line <- returnedIterable
      variable <- line
    }{
      println(variable)
    }
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

  def queryTest(decodeFunctions : ((QuerySolution) => Any)*): List[Iterable[Any]] = { //        System.out.println("about to execute query");
    val queryString = "PREFIX wd: <http://www.wikidata.org/entity/>\nselect ?s ?b where { ?s wd:P27 wd:Q30 .bind(1 as ?b)} LIMIT 100"
    val query = QueryFactory.create(queryString)
    val start = System.currentTimeMillis
    val qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + MyDatasets.dsWikidata + "/query", queryString)
    try {
        val results = qexec.execSelect
        getResultsUsingFunctions(results, decodeFunctions)
      }
     finally qexec.close()
  }

}
