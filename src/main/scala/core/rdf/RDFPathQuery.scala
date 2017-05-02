package core.rdf

import core.globals.MyDatasets
import core.query.Query

/**
  * Created by espen on 02.05.17.
  */
case class RDFPathQuery(queryString : String, isSubjList: Iterable[Boolean], propertyVars : Iterable[String], middleEntityVars: Iterable[String]) {

  def findPaths(query: RDFPathQuery, startEntity: String, endEntity: String, dataset : String = MyDatasets.dsWikidata) : Iterable[RDFPath] = {
    val queryExecutable = new Query(() => query.queryString, dataset)
    queryExecutable.execute()
    val propertyResults : Iterable[List[String]] = query.propertyVars.map(pVar => queryExecutable.getResults(pVar.tail) : List[String])
    val middleEntityResults : Iterable[List[String]] = query.middleEntityVars.map(mVar => queryExecutable.getResults(mVar.tail) : List[String])
    val numberOfPaths = propertyResults.head.length
    Range(0,numberOfPaths).map(index => RDFPath(startEntity, endEntity, propertyResults.map(_(index)).toList, query.isSubjList.toList, middleEntityResults.map(_(index)).toList))
  }

}
