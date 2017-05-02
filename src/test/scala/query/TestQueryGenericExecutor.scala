package query

import core.globals.{KnowledgeGraphs, MyDatasets}
import core.query.QueryGenericExecutor
import core.query.specific.{QueryFactory, QueryStringFactory}
import core.testData.WikidataFactory
import org.scalatest.FunSuite

/**
  * Created by espen on 02.05.17.
  */
class TestQueryGenericExecutor  extends FunSuite{
  test("Should work for ringo starr") {
    val ringoStarr = WikidataFactory.ringoStarr.id
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
    val expectedResult = QueryFactory.findDistinctPropertiesWhereObject(ringoStarr)
    val actualResult= QueryGenericExecutor.executeQuery(QueryStringFactory.distinctPropertiesWhereObject(ringoStarr), List("p"), MyDatasets.dsWikidata)
    import core.query.variables.ResultVariable.getString
    actualResult.head.foreach(rv => assert(expectedResult.contains(getString(rv))))
  }

}
