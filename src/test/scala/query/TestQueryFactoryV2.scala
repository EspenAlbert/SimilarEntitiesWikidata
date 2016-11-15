package query

import org.scalatest.FunSuite
import ownOntologyPopularizer.CustomPropertyClass
import query.specific.QueryFactory

/**
  * Created by Espen on 15.11.2016.
  */
class TestQueryFactoryV2 extends FunSuite{
  test("Find domain count") {
    val a = QueryFactory.findDomainCount("w:P21")
    assert(a == 3029564)
  }
  test("getStrategies") {
    val strategies = QueryFactory.getStrategies("w:P21")
    print(strategies)
    assert(strategies.length == 3)
  }
  test("find all distinct properties") {
    val properties = QueryFactory.findAllDistinctProperties
    print(properties)
    assert(properties.length == 2413)
  }
  test("find all subjects and properties for obama ") {
    val subjectsAndProperties = QueryFactory.findSubjectsAndProperties("w:Q76")
    print(subjectsAndProperties(0))
    assert(subjectsAndProperties(0).length == 410)
    assert(subjectsAndProperties(1).length == 410)
  }
  test("find all subjects of type panda, should be 46") {
    val subjects = QueryFactory.findSubjectsOfType("w:Q33602")
    print(subjects)
    assert(subjects.length == 46)
  }
  test("find all objects and properties of obama") {
    val subjectsAndProperties = QueryFactory.findPropertiesAndObjects("w:Q76")
    print(subjectsAndProperties)
    assert(subjectsAndProperties(0).length == 135)
  }
  test("find total count subjects where property = occupation") {
    val count = QueryFactory.findTotalCountSubjectsWhereProperty("w:P106")
    print(count)
    assert(count == 2806595)
  }
  test("find total count subjects where property = occupation, distinct") {
    val count = QueryFactory.findTotalCountSubjectsWhereProperty("w:P106", distinct = true)
    print(count)
    assert(count == 2060349)
  }
  test("find all P properties") {
    val count = QueryFactory.findAllPropertiesOfCustomClass(CustomPropertyClass.baseProperty)
    print(count)
    assert(count.length == 2398)
  }

}
