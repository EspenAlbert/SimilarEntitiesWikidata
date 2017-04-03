package preprocessing.ownOntologyPopularizer

import core.globals._
import iAndO.dump.DumpObject
import org.scalatest.FunSuite
import preprocessing.ownOntologyPopularizer.attributesGenerator.PropTypeToStrategyCreator._
import tags.{ActiveOnceTag, ActiveTag}

import scala.collection.mutable
/**
  * Created by espen on 21.02.17.
  */
class TestProptypeToStrategyGeneratorWikidata extends FunSuite{
  val exampleProperty = "http://wwww.example.com/test/property1"
  val exampleProperty2 = "http://wwww.example.com/test/property2"
  val exampleProperty3 = "http://wwww.example.com/test/property3"

  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("addMetaKnowledgeToDatabase should work", ActiveTag) {
    addMetaKnowledgeToDatabase(mutable.Map[String, Int]((exampleProperty -> 10)), mutable.Map[String, Int]((exampleProperty -> 20)))
  }
  test("addStrategiesToDatabase", ActiveTag) {
    val stringToType = Map[String, PropertyType]((exampleProperty -> ItemPropertyType()), (exampleProperty2 -> UrlPropertyType()), (exampleProperty3 -> DateTimePropertyType()))
    val stringSet = mutable.Set(exampleProperty)
    val dateSet = mutable.Set(exampleProperty3)
    addStrategiesToDatabase(stringToType, stringSet, stringSet, stringSet, dateSet)
  }
  test("findMetaPropertyKnowledge", ActiveTag) {
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
    val sameTypes = Set("http://www.wikidata.org/entity/P1393", "http://www.wikidata.org/entity/P1072")
    val sharableDomain = Set("http://www.wikidata.org/entity/P1393", "http://www.wikidata.org/entity/P1072")
    val (propToDomainCount: mutable.Map[String, Int], propToRangeCount: mutable.Map[String, Int], sameTypePossibleProps: mutable.Set[String], sharableDomainProps: mutable.Set[String], sharableRangeProps: mutable.Set[String], dateTimeStrategies : mutable.Set[String]) = findMetaPropertyKnowledge(mapPropToPropType)
    assert(propToDomainCount.size == mapPropToPropType.size)
    assert(sameTypePossibleProps == sameTypes)
    assert(propToRangeCount.size == 3)
    assert(sharableDomainProps == sharableDomain)
    assert(sharableRangeProps.size == 1)
    assert(dateTimeStrategies.size == 1)
  }
  test("full generation", ActiveOnceTag) {
    val propToType = DumpObject.readJsonMapStringPropertyType("wikidata-propToTypeMapping")
    generateMetaStatementKnowledgeAndStrategiesForProperties(propToType)
  }
  test("Baseline strategy creation for all item properties", ActiveOnceTag) {
    val propToType = DumpObject.readJsonMapStringPropertyType("wikidata-propToTypeMapping")
    val strategyURIs = List[String](SimilarPropertyOntology.searchDirectedL1Strategy, SimilarPropertyOntology.searchDirectedL2Strategy,
    SimilarPropertyOntology.searchUndirectedL1Strategy, SimilarPropertyOntology.searchUndirectedL2Strategy)
    strategyURIs.foreach(sURI => addStrategyForAllItemProperties(propToType, sURI))
  }

}
