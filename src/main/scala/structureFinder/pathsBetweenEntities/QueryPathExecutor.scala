package structureFinder.pathsBetweenEntities

import core.globals.MyDatasets
import core.query.Query
import structureFinder.KConnectivitySparqlBuilder.{Path, PathQuery}

/**
  * Created by espen on 28.04.17.
  */
object QueryPathExecutor {


  def findPaths(query: PathQuery, startEntity: String, endEntity: String) : Iterable[Path] = {
    val queryExecutable = new Query(() => query.queryString, MyDatasets.dsWikidata)
    queryExecutable.execute()
    val propertyResults : Iterable[List[String]] = query.propertyVars.map(pVar => queryExecutable.getResults(pVar.tail) : List[String])
    val middleEntityResults : Iterable[List[String]] = query.middleEntityVars.map(mVar => queryExecutable.getResults(mVar.tail) : List[String])
    val numberOfPaths = propertyResults.head.length
    Range(0,numberOfPaths).map(index => Path(startEntity, endEntity, propertyResults.map(_(index)).toList, query.isSubjList.toList, middleEntityResults.map(_(index)).toList))
  }

}
