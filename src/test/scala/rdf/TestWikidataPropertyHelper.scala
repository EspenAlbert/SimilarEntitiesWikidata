package rdf

import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestWikidataPropertyHelper extends FunSuite{
  test("The id of a property should be returned properly") {
    val property = "w:P169"
    assert(WikidataPropertyHelper.getId(property) == 169)
  }
  test("If it is not a normal property, an exception is thrown") {
    val property = "rdf:type"
    val ex = intercept[Exception] { WikidataPropertyHelper.getId(property)}
    assert(ex.getMessage == "couldn't find an id for property: " + property)
  }


}
