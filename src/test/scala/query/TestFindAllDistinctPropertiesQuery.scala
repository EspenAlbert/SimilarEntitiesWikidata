package query

import org.scalatest.FunSuite
import query.specific.QueryFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllDistinctPropertiesQuery extends FunSuite{
  test("In total there should be: # of properties") {
    val properties: List[String] = QueryFactory.findAllDistinctProperties
    assert(properties.length == 2413)
    print(properties)
  }



}
