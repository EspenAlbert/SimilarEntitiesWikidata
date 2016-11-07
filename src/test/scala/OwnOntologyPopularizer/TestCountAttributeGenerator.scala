package OwnOntologyPopularizer

import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.CountAttributeGenerator

/**
  * Created by Espen on 02.11.2016.
  */
class TestCountAttributeGenerator extends FunSuite{
  test("There should be no errors") {
    val query = CountAttributeGenerator.generateCounts()
//    query.execute()
//    val properties: ArrayBuffer[String] = query.getSubjects()
//    assert(properties.length == 2398)
//    print(properties)
  }


}
