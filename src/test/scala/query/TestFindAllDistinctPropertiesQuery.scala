package query

import globals.MyDatasets
import org.scalatest.FunSuite
import ownOntologyPopularizer.MapPropertyDatatypeToClass
import query.specific.{QueryFactory}

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllDistinctPropertiesQuery extends FunSuite{
  test("In total there should be: # of properties") {
    val properties: List[String] = QueryFactory.findAllDistinctProperties
    assert(properties.length == 2413)
    print(properties)
  }
  test("All datatypes must be able to find an ID") {
    for(value <- MapPropertyDatatypeToClass.datatypeToClass.values) {
      QueryFactory.dataset = MyDatasets.SimilarProperties
      val propertyId = QueryFactory.findIDForPropertyLabelQuery(value)
      assert(propertyId.length > 0)
      print(propertyId)
    }
  }


}
