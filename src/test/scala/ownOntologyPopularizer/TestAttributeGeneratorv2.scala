package ownOntologyPopularizer

import dump.DumpObject
import globals._
import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.AttributeGeneratorv2._

import scala.collection.mutable
/**
  * Created by espen on 21.02.17.
  */
class TestAttributeGeneratorv2 extends FunSuite{
  val exampleProperty = "http://wwww.example.com/test/property1"
  val exampleProperty2 = "http://wwww.example.com/test/property2"
  val exampleProperty3 = "http://wwww.example.com/test/property3"
  test("addMetaKnowledgeToDatabase should work") {
    addMetaKnowledgeToDatabase(mutable.Map[String, Int]((exampleProperty -> 10)), mutable.Map[String, Int]((exampleProperty -> 20)))
  }
  test("addStrategiesToDatabase") {
    val stringToType = Map[String, PropertyType]((exampleProperty -> ItemPropertyType()), (exampleProperty2 -> UrlPropertyType()), (exampleProperty3 -> DateTimePropertyType()))
    val stringSet = mutable.Set(exampleProperty)
    val dateSet = mutable.Set(exampleProperty3)
    addStrategiesToDatabase(stringToType, stringSet, stringSet, stringSet, dateSet)
  }
  test("findMetaPropertyKnowledge") {
    val directLinkProperty = "http://www.wikidata.org/entity/P1393"
    val valueMatchSubject = "http://www.wikidata.org/entity/P1026"
    val valueMatchObject = "http://www.wikidata.org/entity/P1072"
    val dateProperty = "http://www.wikidata.org/entity/P575"
    val quantityProp = "http://www.wikidata.org/entity/P1971"
    val mapPropToPropType = Map[String, PropertyType](
      (directLinkProperty -> ItemPropertyType()),
      (valueMatchObject -> ItemPropertyType()),
      (dateProperty -> DateTimePropertyType()),
      (valueMatchSubject -> ItemPropertyType()),
      (quantityProp -> QuantityPropertyType())
    )
    val (propToDomainCount: mutable.Map[String, Int], propToRangeCount: mutable.Map[String, Int], sameTypePossibleProps: mutable.Set[String], sharableDomainProps: mutable.Set[String], sharableRangeProps: mutable.Set[String], dateTimeStrategies : mutable.Set[String]) = findMetaPropertyKnowledge(mapPropToPropType)
    assert(propToDomainCount.size == mapPropToPropType.size)
    assert(sameTypePossibleProps.size == 3)
    assert(propToRangeCount.size == 3)
    assert(sharableDomainProps.size == 3)
    assert(sharableRangeProps.size == 1)
    assert(dateTimeStrategies.size == 1)
  }
  test("full generation") {
    val propToType = DumpObject.readJsonMapStringPropertyType("propToTypeMapping")
    generateMetaStatementKnowledgeAndStrategiesForProperties(propToType)
  }

}
