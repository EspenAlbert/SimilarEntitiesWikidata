package OwnOntologyPopularizer

import org.scalatest.FunSuite
import query.specific.QueryFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllPropertiesOfCustomClass extends FunSuite{
  test("In total there should be: 2398# of properties") {
    val properties = QueryFactory.findAllPropertiesOfCustomClass()
    assert(properties.length == 2398)
    print(properties)
  }


}
