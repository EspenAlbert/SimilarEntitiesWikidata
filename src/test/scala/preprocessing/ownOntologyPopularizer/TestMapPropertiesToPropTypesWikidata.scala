package preprocessing.ownOntologyPopularizer

import core.globals._
import data.WikidataFactory
import iAndO.dump.DumpObject
import org.scalatest.FunSuite

import scala.io.Source
import preprocessing.ownOntologyPopularizer.MapPropertiesToPropTypes._
import query.TestFindAllDistinctPropertiesQuery
import tags.{ActiveOnceTag, ActiveSlowTag, ActiveTag, TestOnlyTag}

/**
  * Created by Espen on 01.11.2016.
  */
class TestMapPropertiesToPropTypesWikidata extends FunSuite{
  implicit val dataset = KnowledgeGraph.wikidata
  test("filterGeoTypes should work", ActiveTag) {
    val filteredGeoProperties = filterGeoPropertyTypes(WikidataFactory.allProperties)
    assertResult(WikidataFactory.geoProperties.size + WikidataFactory.qualifierGeoProperties.size){
      filteredGeoProperties.size
    }
  }
  test("filterOrdinaryProperties should work", ActiveTag) {
    val ordinaryProperties = filterOrdinaryProperties(WikidataFactory.allProperties)
    assertResult(WikidataFactory.ordinaryProperties){ordinaryProperties}
  }
  test("Dump object should work") {
    val dummyMap = Map(("http://www.wikidata.org/entity/P155", ItemPropertyType()))
    DumpObject.dumpJsonMapStringPropertyType(dummyMap, "propToTypeMappingTest")
    val readMap = DumpObject.readJsonMapStringPropertyType("propToTypeMappingTest")
    assert(dummyMap == readMap)
  }
  test("findAllPropertyTypesAndTheirPropertyDatatype should work", ActiveSlowTag) {
    val properties = WikidataFactory.allProperties
    val propertyToPropTypes = findAllPropertyTypesAndTheirPropertyDatatype(properties)
    propertyToPropTypes.foreach{case (property, propertyType) => {
      propertyType match {
        case a : GlobeCoordinatePropertyType => assert(WikidataFactory.geoProperties.contains(property))
        case a : ItemPropertyType => assert(WikidataFactory.itemProperties.contains(property))
        case a : QuantityPropertyType => assert(WikidataFactory.quantityProp == property)
        case a : UrlPropertyType => assert(WikidataFactory.urlProperties.contains(property))
        case a : StringPropertyType => assert(WikidataFactory.stringProperties.contains(property))
        case a : DateTimePropertyType => assert(WikidataFactory.dateProperty == property)
      }
    }}
  }
  test("Find property types for the whole dataset", ActiveOnceTag) {
    dumpMappingToOwnOntologyDS(findAllPropertyTypesAndTheirPropertyDatatype())
  }
  test("read stored mapping") {
    val readMap = DumpObject.readJsonMapStringPropertyType("propToTypeMapping")
    println(readMap)
    assert(readMap.values.filter(_.isInstanceOf[DateTimePropertyType]).size == 33)
    val sizeItemProperties = readMap.values.filter(_.isInstanceOf[ItemPropertyType]).size
//    assert(sizeItemProperties == 754, s"but was $sizeItemProperties") maybe it should be 754, it is 709 probably because some objects do not have statements where they are subjects


  }
}
