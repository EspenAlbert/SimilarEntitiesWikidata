package ownOntologyPopularizer

import org.scalatest.FunSuite
import query.specific.QueryFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllPropertiesOfCustomClass extends FunSuite{
  test("In total there should be: 2398# of properties") {
    val properties = QueryFactory.findAllPropertiesOfCustomClass(CustomPropertyClass.baseProperty)
    assert(properties.length == 2398)
    print(properties)
  }
  test("In total there should be less than 800 itemProperties properties") {
    val properties = QueryFactory.findAllPropertiesOfCustomClass(CustomPropertyClass.itemProperty)
    assert(properties.length < 800)
    print(properties)
  }


}
