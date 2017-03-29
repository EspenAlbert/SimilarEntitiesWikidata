package interlink

import core.interlinking.Interlink
import data.DBpediaFactory
import org.scalatest.FunSuite
import tags.ActiveTag

/**
  * Created by espen on 28.03.17.
  */
class TestInterlink extends FunSuite{
  test("dbPedia to wikidata id", ActiveTag) {
    val actual = Interlink.fromDBpediaToWikidata(DBpediaFactory.wales.dbpediaId)
    val expected = DBpediaFactory.wales.wikidataId
    assert(actual == expected)
  }

}
