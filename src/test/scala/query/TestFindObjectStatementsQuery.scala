package query

import org.scalatest.FunSuite
import query.specific.FindObjectStatementsQuery

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindObjectStatementsQuery extends FunSuite{
  test("Object statements for obama should be 135") {
    val obamaId = "Q76"
    val query = new FindObjectStatementsQuery(obamaId)
    query.execute()
    val objects: ArrayBuffer[String] = query.getObjects()
    assert(objects.length == 135)
    print(objects)
  }
  test("Object statements should have properties always starting with P, and for obama have 410") {
    val obamaId = "Q76"
    val query = new FindObjectStatementsQuery(obamaId)
    query.execute()
    val properties: ArrayBuffer[String] = query.getProperties()
    assert(!properties.exists(_.startsWith("w:Q")))
    assert(properties.length == 135)
    print(properties)

  }

}
