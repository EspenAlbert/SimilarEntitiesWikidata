package ownOntologyPopularizer

import org.scalatest.FunSuite

/**
  * Created by Espen on 01.11.2016.
  */
class TestPropertyDatatypeReader extends FunSuite{
  test("reading datatypes should return a valid list") {
    val map = PropertyDatatypeReader.getPropertyDatatypeMap()
    assert(map("P9") == "wikibase-item")
    print(map("P9"))
  }
}
