package query

import java.net.SocketTimeoutException

import core.globals.KnowledgeGraph
import core.query.specific.AskQuery
import org.scalatest.FunSuite
import core.query.specific.QueryFactory._
import data.WikidataFactory
import similarityFinder.MyConfiguration
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
  private val wd = WikidataFactory
  private val ringoStarr = wd.ringoStarr
  test("find subject count for male gender", ActiveTag) {
    val expectedCount = wd.countMaleGender
    val actualCount = findCountForPropertyWithValue(ringoStarr.genderProp, ringoStarr.genderValue)
    assert(expectedCount == actualCount.getOrElse(throw new Exception("Failed to find count")))
  }
  test("find value count for subject=Ringo starr property = occupation", ActiveTag) {
    val expectedCount = ringoStarr.occupationValues.length
    val actualCount = findCountForPropertyWithSubject(ringoStarr.occupationProp, ringoStarr.id)
    assert(expectedCount == actualCount.getOrElse(throw new Exception("Failed to find count")))
  }
  test("find members of the beatles (subjectsOfTypeWithPropertyAndValue)", ActiveTag) {
    val beatles = wd.theBeatles
    val expectedMembers = beatles.members
    val actualMembers = subjectsOfTypeWithPropertyAndValue(ringoStarr.memberOfProp, beatles.id, ringoStarr.rdfTypes)
    actualMembers.foreach(m => assert(expectedMembers.contains(m)))
    expectedMembers.foreach(m => assert(actualMembers.contains(m)))
  }
  test("findObjectsOfTypeForProperty where the type is kommune in norway, P19 = place of birth", ActiveSlowTag) {
    val (_, types) = wd.kristiansandAndTypes()
    val expectedTypes = types.tail //Commune in norway
    val objects = findObjectsOfTypeForProperty(wd.placeOfBirthProp, expectedTypes)
    objects.foreach(o => AskQuery.subjectHasType(o, expectedTypes))
  }
  test("findObjectsHavingMoreThanXStatementsOfProperty", ActiveTag) {
    val songCountThreshold = 50
    val property = ringoStarr.performerProp
    ringoStarr.performedByMostCommonTypes.foreach(pByType => {
      val moreThan50PerformancesEntities = findObjectsHavingMoreThanXStatementsOfPropertyWithType(5, property, pByType)
      assert(moreThan50PerformancesEntities.contains(ringoStarr.id))
    })
  }
  test("findObjectsHavingMoreThanXStatementsOfPropertyWhereSubjectsAgainHaveObjectValue", ActiveTag) {
    val thresholdCount = 2
    val expected = ringoStarr.otherPerformersWithMoreThan2RockMusicPerformances
    val actual = findObjectsHavingMoreThanXStatementsOfPropertyWhereSubjectsAgainHaveObjectValue(thresholdCount, ringoStarr.performerProp, (wd.genre, wd.rockMusicGenre))
    expected.foreach(performer => {
      assert(actual.contains(performer))
    })
  }
  test("findMostCommonTypesOneStepUpInHierarchyForPropertyAndObject", ActiveTag) {
    val expected = ringoStarr.performedByMostCommonTypes
    val actual = findMostCommonTypesOneStepUpInHierarchyForPropertyAndObject(ringoStarr.performerProp, ringoStarr.id)
    expected.foreach(s => assert(actual.contains(s)))
  }
  test("Find object counts for subjects with entity as object", ActiveTag) {
    val actual = findPropertyObjectPairsCountsForSubjectsHavingEntityAsObjectForProperty(ringoStarr.performerProp, ringoStarr.id)
    val expected = ringoStarr.performerSubjectsMostPopularObjects
    expected.foreach(tripple => {
      assert(actual.contains(tripple))
    })
  }
  test("findPropertyObjectPairsCountsForObjectsHavingEntityAsSubjectForProperty, the occupations for ringo starrs most popular objects", ActiveTag) {
    val actual = findPropertyObjectPairsCountsForObjectsHavingEntityAsSubjectForProperty(ringoStarr.occupationProp, ringoStarr.id)
    val expected = ringoStarr.occupationPropsMostPopularObjects
    assert(actual == expected)
  }
  test("findSubjectsHavingMoreThanXStatementsOfPropertyWhereSubjectsAgainHaveObjectValue, other people having more than 4 occupations", ActiveSlowTag) {
    val actual = findSubjectsHavingMoreThanXStatementsOfPropertyWhereSubjectsAgainHaveObjectValue(4, ringoStarr.occupationProp, (wd.typeProperty, wd.professionType))
    assert(actual.contains(ringoStarr.id))
  }
  test("findDistinctPropertiesWhereObject for ringo starr", ActiveTag) {
    val actual = findDistinctPropertiesWhereObject(ringoStarr.id)
    val expected = List(ringoStarr.performerProp, ringoStarr.spouseProp)
    assert(expected.forall(actual.contains(_)))
  }

}
