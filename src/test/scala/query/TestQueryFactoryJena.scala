package query

import core.globals.{KnowledgeGraphs, MyDatasets}
import core.query.ValueMatchCountFinder
import core.query.specific.{AskQuery, QueryFactory, QueryFactoryJena, UpdateQueryFactory}
import core.rdf.TypeCounter
import core.testData.WikidataFactory
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

  test("subjectsWithPropertyAndValue"){//TODO: Test
    val ringoStarrSongs = QueryFactoryJena.subjectsWithPropertyAndValue(wd.ringoStarr.performerProp, wd.ringoStarr.id)
    assert(ringoStarrSongs.size > 50)
    assert(ringoStarrSongs.contains(wd.ringoStarr.performerSubject1))
  }
  test("objectsWithPropertyAndSubject"){//TODO: Test
    val performersOfRingoStarrSong = QueryFactoryJena.objectsWithPropertyAndSubject(wd.ringoStarr.performerProp, wd.ringoStarr.performerSubject1)
    assert(performersOfRingoStarrSong.head == wd.ringoStarr.id)
  }
  test("propertiesAndCountsForType") {//TODO: Test
    val domainTypesForRockBand = QueryFactoryJena.propertiesAndCountsForType(wd.rockBand, true, 0)
    assert(domainTypesForRockBand.size > 100)
    val rangeTypesForRockBand= QueryFactoryJena.propertiesAndCountsForType(wd.rockBand, false, 0)
    assert(rangeTypesForRockBand.size > 5)

  }


}
