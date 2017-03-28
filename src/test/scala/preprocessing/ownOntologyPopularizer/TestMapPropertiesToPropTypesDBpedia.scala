package preprocessing.ownOntologyPopularizer

import core.globals._
import data.{DBpediaFactory, WikidataFactory}
import iAndO.dump.DumpObject
import org.scalatest.FunSuite
import preprocessing.ownOntologyPopularizer.MapPropertiesToPropTypes.{dumpMappingToOwnOntologyDS, findAllPropertyTypesAndTheirPropertyDatatype}
import tags.{ActiveOnceTag, ActiveSlowTag, ActiveTag}

/**
  * Created by espen on 28.03.17.
  */
class TestMapPropertiesToPropTypesDBpedia extends FunSuite{
  val ringoStarr = DBpediaFactory.ringoStarr
  implicit val knowledgeGraph = KnowledgeGraph.dbPedia
  test("findAllPropertyTypesAndTheirPropertyDatatype should work", ActiveSlowTag) {
    val expectedMap = DBpediaFactory.expectedPropertyMapping
    val actualMap = MapPropertiesToPropTypes.findAllPropertyTypesAndTheirPropertyDatatype(expectedMap.keys.toList)
    assert(actualMap == expectedMap)
  }
  test("filterGeoPropertyTypes", ActiveTag) {
    val expected = List(DBpediaFactory.latitudeProperty, DBpediaFactory.longitudeProperty)
    val before = List(DBpediaFactory.dateProperty) ++ expected
    val after = MapPropertiesToPropTypes.filterGeoPropertyTypes(before)
    assert(after == expected)
  }
  test("Find property types for the whole dataset", ActiveOnceTag) {
    dumpMappingToOwnOntologyDS(findAllPropertyTypesAndTheirPropertyDatatype())
  }
  test("Read the dumped datatypes") {
    val propToTypeFilename = DBpediaFactory.propToTypeFilename
    val mapping = DumpObject.readJsonMapStringPropertyType(propToTypeFilename)
    assert(mapping.size == 1375)
  }
}
