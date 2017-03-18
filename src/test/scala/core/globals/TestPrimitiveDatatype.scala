package core.globals

import org.scalatest.FunSuite
import core.globals.PrimitiveDatatype._
import core.globals.{DateTimePropertyType, ItemPropertyType, QuantityPropertyType, StringPropertyType, UrlPropertyType}
/**
  * Created by espen on 17.02.17.
  */
class
TestPrimitiveDatatype  extends FunSuite {
  test("getDatatypeAsStringFromResult") {
    val test1 = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.wikidata.org/ontology#latitude> \"57.0\"^^<http://www.w3.org/2001/XMLSchema#double> ."
    val test2 = "fda <http://www.w3.org/1999/02/22-rdf-syntax-ns#langString>"
    assert(getDatatypeAsStringFromResult(test1).getOrElse("fail") == "http://www.w3.org/2001/XMLSchema#double")
    assert(getDatatypeAsStringFromResult(test2).getOrElse("fail") == "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")
  }
  test("getYear from datatime") {
    val expected1 = getYearFromDateFormat("\"1830\"^^<http://www.w3.org/2001/XMLSchema#gYear>")
    val expected2 = getYearFromDateFormat("\"1830\"^^<http://www.w3.org/2001/XMLSchema#date>")
    val expected3 = getYearFromDateFormat("\"1830\"^^<http://www.w3.org/2001/XMLSchema#gYearMonth>")
    List(expected1, expected2, expected3).foreach((e) => assert(e.getOrElse(1) == 1830))
  }
  test("getPropertyTypeFromDatatypes") {
    val prefixXsd = "http://www.w3.org/2001/XMLSchema#"
    val numericTypes = List(
      "float", "double", "decimal"
    ).map(prefixXsd + _)
    val dateTypes = List(
      "gYearMonth","date", "gYear"
    ).map(prefixXsd + _)
    val stringTypes = List(
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString",
      "http://www.w3.org/2001/XMLSchema#string"
    )
    for(d<- numericTypes) {
      assert(getPropertyTypeFromDatatypes(List(d)).getOrElse("Fail").isInstanceOf[QuantityPropertyType])
    }
    for(d<- dateTypes) {
      assert(getPropertyTypeFromDatatypes(List(d)).getOrElse("Fail").isInstanceOf[DateTimePropertyType])
    }
    for(d<- stringTypes) {
      assert(getPropertyTypeFromDatatypes(List(d)).getOrElse("Fail").isInstanceOf[StringPropertyType])
    }
  }
  test("determineFromObjectValuePropertyType wikidata") {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val optionForPropTypeExpectItempPropType = determineFromObjectValuePropertyType("http://www.wikidata.org/entity/P6")
    assert(optionForPropTypeExpectItempPropType.getOrElse("faill").isInstanceOf[ItemPropertyType])
    val optionForPropTypeExpectIString = determineFromObjectValuePropertyType("http://www.wikidata.org/entity/P1036")
    assert(optionForPropTypeExpectIString.getOrElse("faill").isInstanceOf[StringPropertyType])
    val optionForPropTypeExpectUrlType = determineFromObjectValuePropertyType("http://www.wikidata.org/entity/P1581")
    assert(optionForPropTypeExpectUrlType.getOrElse("faill").isInstanceOf[UrlPropertyType])
  }

}
