package query

import org.scalatest.FunSuite
import ownOntologyPopularizer.CustomPropertyClass
import query.specific.QueryFactoryV2

/**
  * Created by Espen on 15.11.2016.
  */
class TestQueryFactoryV2 extends FunSuite{
  test("Find domain count") {
    val a = QueryFactoryV2.findDomainCount("w:P21")
    assert(a == 3029564)
  }
  test("getStrategies") {
    val strategies = QueryFactoryV2.getStrategies("w:P21")
    print(strategies)
    assert(strategies.length == 3)
  }
  test("find all distinct properties") {
    val properties = QueryFactoryV2.findAllDistinctProperties
    print(properties)
    assert(properties.length == 2413)
  }
  test("find all subjects and properties for obama ") {
    val subjectsAndProperties = QueryFactoryV2.findSubjectsAndProperties("w:Q76")
    print(subjectsAndProperties(0))
    assert(subjectsAndProperties(0).length == 410)
    assert(subjectsAndProperties(1).length == 410)
  }
  test("find all subjects of type panda, should be 46") {
    val subjects = QueryFactoryV2.findSubjectsOfType("w:Q33602")
    print(subjects)
    assert(subjects.length == 46)
  }
  test("find all objects and properties of obama") {
    val subjectsAndProperties = QueryFactoryV2.findPropertiesAndObjects("w:Q76")
    print(subjectsAndProperties)
    assert(subjectsAndProperties(0).length == 135)
  }
  test("find total count subjects where property = occupation") {
    val count = QueryFactoryV2.findTotalCountSubjectsWhereProperty("w:P106")
    print(count)
    assert(count == 2806595)
  }
  test("find total count subjects where property = occupation, distinct") {
    val count = QueryFactoryV2.findTotalCountSubjectsWhereProperty("w:P106", distinct = true)
    print(count)
    assert(count == 135)
  }
  test("find all P properties") {
    val count = QueryFactoryV2.findAllPropertiesOfCustomClass(CustomPropertyClass.baseProperty)
    print(count)
    assert(count.length == 2398)
  }

}
