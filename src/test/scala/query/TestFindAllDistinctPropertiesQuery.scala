package query

import org.scalatest.FunSuite
import ownOntologyPopularizer.MapPropertyDatatypeToClass
import query.specific.FindAllDistinctPropertiesQuery
import query.specific.ontologyQueries.FindIDForPropertyLabelQuery

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllDistinctPropertiesQuery extends FunSuite{
  test("In total there should be: # of properties") {
    val query = new FindAllDistinctPropertiesQuery()
    query.execute()
    val properties: ArrayBuffer[String] = query.getProperties()
    assert(properties.length == 2413)
    print(properties)
  }
  test("All datatypes must be able to find an ID") {
    for(value <- MapPropertyDatatypeToClass.datatypeToClass.values) {
      val query = new FindIDForPropertyLabelQuery(value)
      query.execute()
      assert(query.getValue().length > 0)
      print(query.getValue())
    }
  }


}
