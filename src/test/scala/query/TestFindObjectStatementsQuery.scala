package query

import org.scalatest.FunSuite
import query.specific.QueryFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindObjectStatementsQuery extends FunSuite{
  test("Object statements for obama should be 135") {
    val obamaId = "Q76"
    val objects: List[String] = QueryFactory.findSubjectsAndProperties(obamaId)(0)
    assert(objects.length == 135)
    print(objects)
  }
  test("Subject statements should have properties always starting with P, and for obama have 135") {
    val obamaId = "Q76"
    val properties: List[String] = QueryFactory.findSubjectsAndProperties(obamaId)(1)
    assert(!properties.exists(_.startsWith("w:Q")))
    assert(properties.length == 135)
    print(properties)

  }

}
