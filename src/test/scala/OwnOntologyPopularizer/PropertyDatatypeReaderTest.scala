package OwnOntologyPopularizer

import org.scalatest.FunSuite

/**
  * Created by Espen on 01.11.2016.
  */
class PropertyDatatypeReaderTest extends FunSuite{
  test("reading datatypes should return a valid list") {
    val pdr = new PropertyDatatypeReader()
    val map = pdr.getPropertyDatatypeMap()
    assert(map.get("P9") == Some("wikibase-item"))
    print(map("P9"))
  }
}
