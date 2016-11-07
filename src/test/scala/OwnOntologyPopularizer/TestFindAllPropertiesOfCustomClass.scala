package OwnOntologyPopularizer

import org.scalatest.FunSuite
import query.specific.ontologyQueries.FindAllPropertiesOfCustomClass

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllPropertiesOfCustomClass extends FunSuite{
  test("In total there should be: 2398# of properties") {
    val query = new FindAllPropertiesOfCustomClass()
    query.execute()
    val properties: List[String] = query.getSubjects()
    assert(properties.length == 2398)
    print(properties)
  }


}
