package query

import core.globals.KnowledgeGraph
import org.scalatest.FunSuite
import core.query.specific.QueryFactory._
/**
  * Created by espen on 20.02.17.
  */
class TestQueryFactoryRaw extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("Should be able to find all datatypes for a property") {
    val dTypes = findAllDistinctDatatypesForProperty("http://www.wikidata.org/entity/P6")
    println(dTypes)
    assert(dTypes.length == 1)
    val dTypesTime = findAllDistinctDatatypesForProperty("http://www.wikidata.org/entity/P575")
    println(dTypesTime)
    assert(dTypesTime.length == 3)

  }
  test("findDomainCount") {
    val dCount = findDomainCount("http://www.wikidata.org/entity/P6")
    print(dCount)
    assert(dCount == 10686)
  }

  test("findRangeCount") {
    val rCount = findRangeCount("http://www.wikidata.org/entity/P127")
    println(rCount)
    assert(rCount == 9484)
  }
  test("find max date for http://www.wikidata.org/entity/P575 date of discovery") {
    val maxDate = findMaxDate("http://www.wikidata.org/entity/P575")
    val maxDate2 = findMaxDate("http://www.wikidata.org/entity/P1326")
    println(maxDate)
    println(maxDate2)

  }

}
