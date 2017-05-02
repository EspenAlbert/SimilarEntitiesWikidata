package core.query

/**
  * Created by espen on 02.05.17.
  */

import core.globals.MyDatasets
import org.apache.jena.query.{Query, _}
import org.apache.jena.rdf.model.Resource

import scala.util.{Failure, Success, Try}

object QueryServerScala {
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
      case Failure(_) => -1
    }
  }

  def generateResultDecoderFromMultipleFunctions(rs: QuerySolution => Any) : Unit = {

  }




  def getStreamFromResult[T](results: ResultSet, resultVars : Iterable[String]) :List[Iterable[Any]] = {
    if(results.hasNext) {
      val nextResult = results.next()
        return resultVars.map(rv => rv match {
          case a : String => nextResult.getResource(a).getURI
          case _ => nextResult.getLiteral("s").getString
        }) :: getStreamFromResult(results, resultVars)
    } else {
      Nil
    }
  }
  def getResultsUsingFunction[T](results: ResultSet,resultVars : QuerySolution => T ) : List[T] = {
    if(results.hasNext) {
      resultVars(results.next()) :: getResultsUsingFunction(results, resultVars)
    } else {
      Nil
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

  def queryTest(resultVars : ((QuerySolution) => Any)*): List[Iterable[Any]] = { //        System.out.println("about to execute query");
    val queryString = "PREFIX wd: <http://www.wikidata.org/entity/>\nselect ?s ?b where { ?s wd:P27 wd:Q30 .bind(1 as ?b)} LIMIT 100"
    val query = QueryFactory.create(queryString)
    val start = System.currentTimeMillis
    val qexec = QueryExecutionFactory.sparqlService("http://localhost:3030/" + MyDatasets.dsWikidata + "/query", queryString)
    try {
      var a = 0
      val results = qexec.execSelect
      getResultsUsingFunctions(results, resultVars)
//      while (results.hasNext)
//      {
//        a += 1
//        if (a % 1000 == 0) System.out.println(System.currentTimeMillis - start)
//        val next = results.next
//        val s = next.getResource("s")
//        val uri = s.getURI
//        println(uri)
//                        System.out.println(uri);
      }
      //            ResultSetFormatter.out(outputStream, results, query);
     finally qexec.close()
  }

}
