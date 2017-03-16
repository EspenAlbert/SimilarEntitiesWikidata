package query

import core.query.specific.QueryFactory
import iAndO.dump.DumpObject
import org.scalatest.FunSuite
import org.scalatest.tagobjects.Slow
import tags.Active
/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllDistinctPropertiesQuery extends FunSuite{
  val filename = "/test/distinctPropertiesWikidata"
  test("In total there should be: # of properties", Slow) {
    val properties: List[String] = QueryFactory.findAllDistinctProperties
    DumpObject.dumpListString(properties, filename)
    assert(properties.length == 5379)
  }
  test("Same test, but reading from file", Active ) {
    val properties: List[String] = DumpObject.getListString(filename)
    assert(properties.length == 5379)
  }




}
