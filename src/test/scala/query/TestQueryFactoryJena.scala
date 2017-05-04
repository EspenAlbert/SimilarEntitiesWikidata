package query

import core.globals.KnowledgeGraphs
import core.query.specific.{QueryFactory, QueryFactoryJena}
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



}
