package query

import core.globals.KnowledgeGraphs
import core.query.ValueMatchCountFinder
import core.query.specific.UpdateQueryFactory
import core.testData.{DBpediaFactory, WikidataFactory}
import org.scalatest.FunSuite
import tags.ActiveTag

/**
  * Created by espen on 04.05.17.
  */
class TestValueMatchCountFinder extends FunSuite{

  test("Should be able to create a count and store it in the database", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
    val ringoStarr = WikidataFactory.ringoStarr
    val expectedCount = WikidataFactory.countMaleGender
    val actualCount = ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(ringoStarr.genderValue, ringoStarr.genderProp, true).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(expectedCount == actualCount)
    val cachedCount = ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(ringoStarr.genderValue, ringoStarr.genderProp, true).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(cachedCount == actualCount)
  }
  test("Should be able to create  for dbpedia and store it in the database", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraphs.dbPedia
    val person = DBpediaFactory.person
    val expectedCount = DBpediaFactory.personCount
    val actualCount = ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(person, KnowledgeGraphs.getTypeProperty(knowledgeGraph), false).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(expectedCount == actualCount)
    val cachedCount = ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(person, KnowledgeGraphs.getTypeProperty(knowledgeGraph), false).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(cachedCount == actualCount)
  }

}
