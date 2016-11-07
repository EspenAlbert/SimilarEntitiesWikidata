package query

import org.scalatest.FunSuite
import query.specific.QueryFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindCountWhereQuery extends FunSuite{
  test("The count for property P2011 should be exactly 540") {
    val propertyId = "w:P2011"
    val count = QueryFactory.findTotalCountWhereProperty(propertyId)
    assert(count == 540)
    print(count)
  }

}
