package query

import core.globals.KnowledgeGraphs
import core.query.ValueMatchCountFinder
import core.testData.WikidataFactory
import org.scalatest.FunSuite
import tags.ActiveTag

/**
  * Created by espen on 04.05.17.
  */
class TestValueMatchCountFinder extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraphs.wikidata
  val ringoStarr = WikidataFactory.ringoStarr

  test("Should be able to create a count and store it in the database", ActiveTag) {
    val expectedCount = WikidataFactory.countMaleGender
    val actualCount = ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(ringoStarr.genderValue, ringoStarr.genderProp, true).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(expectedCount == actualCount)
    val cachedCount = ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(ringoStarr.genderValue, ringoStarr.genderProp, true).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(cachedCount == actualCount)
  }

}
