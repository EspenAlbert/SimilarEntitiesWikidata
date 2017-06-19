package query

import core.globals.{KnowledgeGraphs, MyDatasets}
import core.query.ValueMatchCountFinder
import core.query.specific.{AskQuery, QueryFactory, QueryFactoryJena, UpdateQueryFactory}
import core.rdf.TypeCounter
import core.testData.{DBpediaFactory, WikidataFactory}
import org.scalatest.FunSuite

/**
  * Created by espen on 03.05.17.
  */
class TestQueryFactoryJena extends FunSuite{
  val wd = WikidataFactory
  implicit val knowledgeGraph = KnowledgeGraphs.wikidata
  test("Human type for ringo starr in wikidata") {
    val rStarr = wd.ringoStarr.id
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
    val expected = wd.human
    val actual = QueryFactoryJena.entityTypes(rStarr)
    assert(actual.head == expected)
  }

  test("Timing difference between query factories") {
    val uk = wd.ringoStarr.countryOfCitizenShipValue
    val initialQuery = QueryFactoryJena.distinctPropertiesWhereEntityIsObject(uk)
    val newFactoryTime = Range(1,10).map(i => timeFunction(() => QueryFactoryJena.distinctPropertiesWhereEntityIsObject(uk))).sum
    val oldFactoryTime =  Range(1,10).map(i=>timeFunction(() => QueryFactory.findDistinctPropertiesWhereObject(uk))).sum
    println(s"New factory time: $newFactoryTime")
    println(s"Old factory time: $oldFactoryTime")
    val ringoStarr = wd.ringoStarr.id
    val newFactoryTime2 = Range(1,10).map(i => timeFunction(() => QueryFactoryJena.findSubjectsAndProperties(ringoStarr))).sum
    val oldFactoryTime2 =  Range(1,10).map(i=>timeFunction(() => QueryFactory.findSubjectsAndProperties(ringoStarr))).sum
    println(s"New factory time query2: $newFactoryTime2")
    println(s"Old factory time query2: $oldFactoryTime2")
  }
  def timeFunction(functionTimed : () => Any): Long = {
    val start = System.currentTimeMillis()
    val r = functionTimed()
    return System.currentTimeMillis() - start
  }
  test("parentsTo rock band") {
    val rockband = wd.rockBand
    val expected = List(wd.band, wd.musicalEnsemble)
    val notExpected = List(wd.human)
    val actual = QueryFactoryJena.parentsTo(rockband).toList
    expected.foreach(e => assert(actual.contains(e)))
    notExpected.foreach(e => assert(!actual.contains(e)))
  }
  test("children of band") {
    val band = wd.band
    val expected = List(wd.rockBand)
    val notExpected = List(wd.human, wd.musicalEnsemble)
    val actual = QueryFactoryJena.childrenOf(band)
    expected.foreach(e => assert(actual.contains(e)))
    notExpected.foreach(e => assert(!actual.contains(e)))
  }
  test("finding property distributions locally should work"){
    val distributions = QueryFactoryJena.allPropertyDistributionsLocally(wd.rockBand)
    distributions.foreach(println)
  }
  test("parentToEntityXStepsAway should work for rock band"){
    val firstLevel = QueryFactoryJena.parentToEntityXStepsAway(wd.rockBand,1)
    assert(firstLevel.head == wd.band)
    val actualSecondLevel = QueryFactoryJena.parentToEntityXStepsAway(wd.rockBand, 2)
    assert(actualSecondLevel.toList.contains(wd.musicalEnsemble))
  }
  test("countEntitiesOfTypeForProperty") {
    val humansWithPerformerProp = QueryFactoryJena.countEntitiesOfTypeForProperty(wd.human, false, wd.ringoStarr.performerProp)
    println(humansWithPerformerProp)
    assert(humansWithPerformerProp > 100)
    println(TypeCounter.findGlobalCountOfEntitiesOfType(wd.human))
  }
  test("typesWithMoreThanThresholdEntities") {
    val typesWithCounts = QueryFactoryJena.typesWithMoreThanThresholdEntities(100000)
    assert(typesWithCounts.exists(_._1 == wd.human))
  }
  test("selectTriples") {
    val isEmpty = AskQuery.datasetIsEmpty(MyDatasets.dsWikidata)
    assert(!isEmpty)
  }
  test("datasetSize") {
    val dsValueMatch = QueryFactoryJena.datasetSize(MyDatasets.valueMatchWikidata)
    println(s"value match wd statement size: $dsValueMatch")
    assert(dsValueMatch > 10)
  }

  test("subjectsWithPropertyAndValue"){
    val ringoStarrSongs = QueryFactoryJena.subjectsWithPropertyAndValue(wd.ringoStarr.performerProp, wd.ringoStarr.id)
    assert(ringoStarrSongs.size > 50)
    assert(ringoStarrSongs.contains(wd.ringoStarr.performerSubject1))
  }
  test("objectsWithPropertyAndSubject"){
    val performersOfRingoStarrSong = QueryFactoryJena.objectsWithPropertyAndSubject(wd.ringoStarr.performerProp, wd.ringoStarr.performerSubject1)
    assert(performersOfRingoStarrSong.head == wd.ringoStarr.id)
  }
  test("propertiesAndCountsForType") {
    val domainTypesForRockBand = QueryFactoryJena.propertiesAndCountsForType(wd.rockBand, true, 0)
    assert(domainTypesForRockBand.size > 100)
    val rangeTypesForRockBand= QueryFactoryJena.propertiesAndCountsForType(wd.rockBand, false, 0)
    assert(rangeTypesForRockBand.size > 5)

  }
  test("highValueMatchesForEntity should work for uk") {
    ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(wd.ringoStarr.countryOfCitizenShipValue, wd.ringoStarr.countryOfCitizenShipProperty,false)
    val actual = QueryFactoryJena.highValueMatchesForEntity(wd.ringoStarr.countryOfCitizenShipValue)
    assert(actual.contains(wd.ringoStarr.countryOfCitizenShipProperty))
  }
  test("entity types for ring starr member of should be rock band, entity type for a song of ringo star should be human") {
    val actualRockBand = QueryFactoryJena.entityTypesForValues(wd.ringoStarr.id, wd.ringoStarr.memberOfProp, true)
    assert(actualRockBand.head == wd.rockBand)
    val actualHuman = QueryFactoryJena.entityTypesForValues(wd.ringoStarr.performerSubject1, wd.ringoStarr.performerProp, true)
    assert(actualHuman.contains(wd.human))
    val actualHumanAgain = QueryFactoryJena.entityTypesForValues(wd.theBeatles.id, wd.ringoStarr.memberOfProp, false)
    assert(actualHumanAgain.head == wd.human)
  }
  test("Subjects of entity type for property lifestyle for humans what are the values") {
    val actualSubjects = QueryFactoryJena.subjectsOfEntityTypeForProperty(wd.ringoStarr.lifestyleProp, wd.human, commonType = true)
    assert(actualSubjects.contains(wd.ringoStarr.id))
    assert(actualSubjects.contains(wd.paulMcCartney))
  }
  test("Objects of entity type rock band for member of property") {
    val actualObjects = QueryFactoryJena.objectsOfEntityTypeForProperty(wd.ringoStarr.memberOfProp, wd.rockBand)
    assert(actualObjects.contains(wd.theBeatles.id))
    assert(!actualObjects.contains(wd.paulMcCartney))
  }
  test("Entities of property should work for lifestyle and record label prop") {
    val actualSubjects = QueryFactoryJena.entitiesOfProperty(wd.ringoStarr.lifestyleProp, true)
    assert(actualSubjects.contains(wd.ringoStarr.id))
    val actualObjects = QueryFactoryJena.entitiesOfProperty(wd.ringoStarr.recordLabelProp, false)
    wd.ringoStarr.recordLabelValues.foreach(recordLabel => assert(actualObjects.contains(recordLabel)))
  }
  test("findOrderedCountForTypes") {
    val isDomainProps = List(wd.ringoStarr.recordLabelProp, wd.genre)
    val isRangePrps = List(wd.ringoStarr.performerProp)
    val actualTypes = QueryFactoryJena.findOrderedCountForTypes(isDomainProps, isRangePrps, 20)
    println(actualTypes)
    assert(actualTypes.contains(wd.human))
    val domainForRingoStarr = QueryFactoryJena.propertiesForWhereEntityIsSubject(wd.ringoStarr.id)
    val rangeForRingoStarr = QueryFactoryJena.distinctPropertiesWhereEntityIsObject(wd.ringoStarr.id)
    val actualTypesRingoStarr = QueryFactoryJena.findOrderedCountForTypes(domainForRingoStarr, rangeForRingoStarr, 20)
    println(actualTypesRingoStarr)
    assert(actualTypes.contains(wd.human))
  }
  test("findOrderedSumOfRatioForTypes") {
    val isDomainProps = List(wd.ringoStarr.recordLabelProp, wd.genre)
    val isRangePrps = List(wd.ringoStarr.performerProp)
    val actualTypes = QueryFactoryJena.findOrderedSumOfRatioForTypes(isDomainProps, isRangePrps, 20)
    println(actualTypes)
//    assert(actualTypes.contains(wd.human))
    val domainForRingoStarr = QueryFactoryJena.propertiesForWhereEntityIsSubject(wd.ringoStarr.id, distinct = false)
    val rangeForRingoStarr = QueryFactoryJena.distinctPropertiesWhereEntityIsObject(wd.ringoStarr.id, distinct = false)
    val actualTypesRingoStarr = QueryFactoryJena.findOrderedSumOfRatioForTypes(domainForRingoStarr, rangeForRingoStarr, 50)
    println(actualTypesRingoStarr)
    assert(actualTypes.contains(wd.human))
  }
  test("childrenOfEntityXStepsAway should work for (jazzband, rock band), band, musical ensemble") {
    val expected1StepMuscialEnsemble = wd.band
    val expected2stepsMusicalEnsemble = List(wd.jazzBand, wd.rockBand)
    val actual1Step = QueryFactoryJena.childrenOfEntityXStepsAway(wd.musicalEnsemble,1)
    assert(actual1Step.contains(expected1StepMuscialEnsemble))
    val actual2Steps = QueryFactoryJena.childrenOfEntityXStepsAway(wd.musicalEnsemble, 2)
    expected2stepsMusicalEnsemble.foreach(entityType => assert(actual2Steps.contains(entityType)))
  }
  test("numberOfTypesWithPropertyDistributionLocally") {
    val expectedWikidata = 2473
    val actualWikidata = QueryFactoryJena.numberOfTypesWithPropertyDistributionLocally(KnowledgeGraphs.wikidata)
    assert(expectedWikidata == actualWikidata)
    val expectedDBpedia = 366
    val actualDBpedia = QueryFactoryJena.numberOfTypesWithPropertyDistributionLocally(KnowledgeGraphs.dbPedia)
    assert(expectedDBpedia == actualDBpedia)
  }
  test("subjectsWithObjectsOfEntityTypeForProperty should work for member of rockband"){
    val expectedSubject = wd.ringoStarr.id
    val expectedSubjectObjectValue = wd.ringoStarr.memberOfValue
    val expectedSubjectCount = 200
    val actual = QueryFactoryJena.subjectsWithObjectsOfEntityTypeForProperty(wd.ringoStarr.memberOfProp, wd.rockBand, propertyHasLowCount = false)
    assert(actual.contains((expectedSubject, expectedSubjectObjectValue)))
    assert(actual.size > expectedSubjectCount)
  }
  test("objectsWithSubjectOfEntityTypeForProperty should work for human member of "){
    val actual = QueryFactoryJena.objectsWithSubjectOfEntityTypeForProperty(wd.ringoStarr.memberOfProp, wd.human, true)
    assert(actual.contains((wd.ringoStarr.id,wd.ringoStarr.memberOfValue)))
    assert(actual.size > 1000)
  }
  test("distinct properties where entity is object") {
    implicit val knowledgeGraph = KnowledgeGraphs.dbPedia
    val dbp = DBpediaFactory
    val properties = QueryFactoryJena.distinctPropertiesWhereEntityIsObject(dbp.johnLennon)
    println(properties)
  }
  test("distinct properties where entity is object wikidata ") {
    val properties = QueryFactoryJena.distinctPropertiesWhereEntityIsObject(wd.ringoStarr.id)
    println(properties)
  }
  test("Ringo starr should be connected to the shine silently and vice versa... with counts!") {
    val objectsForShineSilently = QueryFactoryJena.objectsConnectedToSubject(wd.ringoStarr.performerSubject1)
    assert(objectsForShineSilently.contains(wd.ringoStarr.id))
    val subjectsOfRingoStarr = QueryFactoryJena.subjectsConnectedToObject(wd.ringoStarr.id)
    assert(subjectsOfRingoStarr.contains(wd.ringoStarr.performerSubject1))
    val objectsCount = QueryFactoryJena.objectsConnectedToSubjectCount(wd.ringoStarr.performerSubject1)
    val subjectsOfRingoStarrCount = QueryFactoryJena.subjectsConnectedToObjectCount(wd.ringoStarr.id)
    assert(subjectsOfRingoStarrCount > objectsCount)
  }


}
