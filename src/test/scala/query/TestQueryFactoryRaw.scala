package query

import org.scalatest.FunSuite
import query.specific.QueryFactoryRaw._
/**
  * Created by espen on 20.02.17.
  */
class TestQueryFactoryRaw extends FunSuite{
  test("Should be able to find all datatypes for a property") {
    val dTypes = findAllDistinctDatatypesForProperty("http://www.wikidata.org/entity/P6")
    println(dTypes)
    assert(dTypes.length == 1)
    val dTypesTime = findAllDistinctDatatypesForProperty("http://www.wikidata.org/entity/P575")
    println(dTypesTime)
    assert(dTypesTime.length == 3)

  }

}
