package query

import org.scalatest.FunSuite
import query.specific.QueryFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindStrategiesQuery extends FunSuite{
  test("w:P31 should have 4 strategies returned") {
    val property = "w:P31"
    val properties: List[String] = QueryFactory.getStrategies(property)
    assert(properties.length == 4)
    print(properties)
  }


}
