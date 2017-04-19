package query

import java.net.SocketTimeoutException

import core.globals.KnowledgeGraph
import core.query.specific.AskQuery
import org.scalatest.FunSuite
import core.query.specific.QueryFactory._
import core.strategies.ExpandNodeStrategy
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

  test("findDomainTypesForProperty", ActiveTag) {
    val expected = List(wd.human, "http://www.wikidata.org/entity/Q515", "http://www.wikidata.org/entity/Q3464126", "http://www.wikidata.org/entity/Q515", "http://www.wikidata.org/entity/Q839954", "http://www.wikidata.org/entity/Q486972", "http://www.wikidata.org/entity/Q15649510", "http://www.wikidata.org/entity/Q137535", "http://www.wikidata.org/entity/Q13406463", "http://www.wikidata.org/entity/Q6498826", "http://www.wikidata.org/entity/Q15125752", "http://www.wikidata.org/entity/Q182603", "http://www.wikidata.org/entity/Q4167410", "http://www.wikidata.org/entity/Q44613", "http://www.wikidata.org/entity/Q35509", "http://www.wikidata.org/entity/Q571", "http://www.wikidata.org/entity/Q532", "http://www.wikidata.org/entity/Q216353", "http://www.wikidata.org/entity/Q11424", "http://www.wikidata.org/entity/Q159979", "http://www.wikidata.org/entity/Q2985549", "http://www.wikidata.org/entity/Q3375719", "http://www.wikidata.org/entity/Q188784", "http://www.wikidata.org/entity/Q15632617", "http://www.wikidata.org/entity/Q3658341", "http://www.wikidata.org/entity/Q15773317", "http://www.wikidata.org/entity/Q15773347", "http://www.wikidata.org/entity/Q7918273", "http://www.wikidata.org/entity/Q3320743", "http://www.wikidata.org/entity/Q1855011", "http://www.wikidata.org/entity/Q1715155", "http://www.wikidata.org/entity/Q4931504", "http://www.wikidata.org/entity/Q721207", "http://www.wikidata.org/entity/Q374666", "http://www.wikidata.org/entity/Q16998564", "http://www.wikidata.org/entity/Q8046437", "http://www.wikidata.org/entity/Q3445893", "http://www.wikidata.org/entity/Q2369882", "http://www.wikidata.org/entity/Q27301864", "http://www.wikidata.org/entity/Q2642184")
    val actual = findDomainTypesForProperty(ringoStarr.lifestyleProp)
    assert(expected.forall(actual.contains(_)))
  }
  test("findRangeTypesForProperty", ActiveTag) {
    val notExpected = List(wd.human)
    val expected = List("http://www.wikidata.org/entity/Q189533", "http://www.wikidata.org/entity/Q4875688")
    val actual = findRangeTypesForProperty(ringoStarr.lifestyleProp)
    assert(expected.forall(actual.contains(_)))
    assert(notExpected.forall(!actual.contains(_)))
  }
  test("findOrderedCountForTypes", ActiveTag) {
    val expected = List("http://www.wikidata.org/entity/Q15632617", "http://www.wikidata.org/entity/Q5", "http://www.wikidata.org/entity/Q188784")
    val w = wd.w
    val domainProps = List(w + "P21", w + "P26", w + "P40", w + "P19")
    val rangeProps = List(w + "P19")
    val actuals = findOrderedCountForTypes(domainProps, rangeProps).take(10)
    expected.foreach(e => assert(actuals.contains(e)))
  }
  test("objectsWithPropertyAndSubject", ActiveTag) {
    MyConfiguration.useMustHaveProperty = true
    ExpandNodeStrategy.mustHaveProperty = ringoStarr.performerProp
    ExpandNodeStrategy.mustHavePropertyIsSubject = false
    val objects = objectsWithPropertyAndSubject(ringoStarr.spouseProp, ringoStarr.id)
    assert(objects.head == "http://www.wikidata.org/entity/Q233993")
    MyConfiguration.useMustHaveProperty = false
    val objectsWithoutRequirement = objectsWithPropertyAndSubject(ringoStarr.spouseProp, ringoStarr.id)
    assert(objectsWithoutRequirement == ringoStarr.spouseValues)
  }
  test("subjectsWithPropertyAndValue", ActiveTag) {
    MyConfiguration.useMustHaveProperty = true
    ExpandNodeStrategy.mustHaveProperty = ringoStarr.spouseProp
    ExpandNodeStrategy.mustHavePropertyIsSubject = true
    val subjects =subjectsWithPropertyAndValue(ringoStarr.memberOfProp, ringoStarr.memberOfValue)
    assert(!subjects.contains(wd.peteBest))
    MyConfiguration.useMustHaveProperty = false
    val subjectsNoRestriction =subjectsWithPropertyAndValue(ringoStarr.memberOfProp, ringoStarr.memberOfValue)
    assert(wd.theBeatles.members.forall(subjectsNoRestriction.contains(_)))
    println(subjects)
    println(subjectsNoRestriction)

  }

}
