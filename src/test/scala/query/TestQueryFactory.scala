package query

import core.globals.KnowledgeGraph
import core.query.specific.AskQuery
import org.scalatest.FunSuite
import core.query.specific.QueryFactory._
import data.WikidataFactory
import tags.{ActiveSlowTag, ActiveTag}
/**
  * Created by espen on 20.02.17.
  */
class TestQueryFactory extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("Should be able to find all datatypes for a property") {
    val dTypes = findAllDistinctDatatypesForProperty("http://www.wikidata.org/entity/P6")
    println(dTypes)
    assert(dTypes.length == 1)
    val dTypesTime = findAllDistinctDatatypesForProperty("http://www.wikidata.org/entity/P575")
    println(dTypesTime)
    assert(dTypesTime.length == 3)

  }
  test("findDomainCount") {
    val dCount = findDomainCount("http://www.wikidata.org/entity/P6")
    print(dCount)
    assert(dCount == 10686)
  }

  test("findRangeCount") {
    val rCount = findRangeCount("http://www.wikidata.org/entity/P127")
    println(rCount)
    assert(rCount == 9484)
  }
  test("find max date for http://www.wikidata.org/entity/P575 date of discovery") {
    val maxDate = findMaxDate("http://www.wikidata.org/entity/P575")
    val maxDate2 = findMaxDate("http://www.wikidata.org/entity/P1326")
    println(maxDate)
    println(maxDate2)
  }
  test("find subject count for male gender", ActiveTag) {
    val expectedCount = WikidataFactory.countMaleGender
    val actualCount = findCountForPropertyWithValue(WikidataFactory.ringoStarr.genderProp, WikidataFactory.ringoStarr.genderValue)
    assert(expectedCount == actualCount.getOrElse(throw new Exception("Failed to find count")))
  }
  test("find value count for subject=Ringo starr property = occupation", ActiveTag) {
    val expectedCount = WikidataFactory.ringoStarr.occupationValues.length
    val actualCount = findCountForPropertyWithSubject(WikidataFactory.ringoStarr.occupationProp, WikidataFactory.ringoStarr.id)
    assert(expectedCount == actualCount.getOrElse(throw new Exception("Failed to find count")))
  }
  test("find members of the beatles (subjectsOfTypeWithPropertyAndValue)", ActiveTag) {
    val beatles = WikidataFactory.theBeatles
    val expectedMembers = beatles.members
    val ringoStarr = WikidataFactory.ringoStarr
    val actualMembers = subjectsOfTypeWithPropertyAndValue(ringoStarr.memberOfProp, beatles.id, ringoStarr.rdfTypes)
    actualMembers.foreach(m => assert(expectedMembers.contains(m)))
    expectedMembers.foreach(m => assert(actualMembers.contains(m)))
  }
  test("findObjectsOfTypeForProperty where the type is kommune in norway, P19 = place of birth", ActiveSlowTag) {
    val (_, types) = WikidataFactory.kristiansandAndTypes()
    val expectedTypes = types.tail //Commune in norway
    val objects = findObjectsOfTypeForProperty(WikidataFactory.placeOfBirthProp, expectedTypes)
    objects.foreach(o => AskQuery.subjectHasType(o, expectedTypes))
  }

}
