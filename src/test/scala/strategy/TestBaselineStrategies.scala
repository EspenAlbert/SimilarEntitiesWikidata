package strategy

import core.globals.{KnowledgeGraph, SimilarPropertyOntology}
import core.strategies.{SearchDirectedL1Strategy, SearchDirectedL2Strategy, SearchUndirectedL1Strategy, SearchUndirectedL2Strategy}
import data.WikidataFactory
import org.scalatest.FunSuite
import similarityFinder.{MyConfiguration, SimilarityFinder2}
import tags.{ActiveSlowTag, ActiveTag}

/**
  * Created by espen on 31.03.17.
  */
class TestBaselineStrategies extends FunSuite{
  val wd = WikidataFactory
  val rStarr = wd.ringoStarr
  implicit val knowledgeGraph = KnowledgeGraph.wikidata


  test("SearchDirectL1 should work", ActiveTag) {
    val expectedSimilars = Set(rStarr.spouseValues: _*)
    val strategy = SearchDirectedL1Strategy(rStarr.spouseProp, rStarr.spouseValues)
    val actualSimilars = strategy.findSimilars().keySet
    assert(expectedSimilars == actualSimilars)
  }
  test("SearchDirectL2 should work", ActiveTag) {
    val expectedSimilarsL1 = Set(rStarr.spouseValues: _*)
    val strategy = SearchDirectedL2Strategy(rStarr.spouseProp, rStarr.spouseValues)
    val actualSimilars = strategy.findSimilars().keySet
    assert(expectedSimilarsL1.size < actualSimilars.size)
  }
  test("SearchUndirectedL1 should work", ActiveTag) {
    val strategy = SearchUndirectedL1Strategy(rStarr.performerProp, List(rStarr.performerSubject1))
    val expectedSimilars = Set(rStarr.performerSubject1)
    val actualSimilars = strategy.findSimilars().keySet
    assert(expectedSimilars == actualSimilars)
  }
  test("SearchUndirectL2 should work", ActiveTag) {
    val expectedSimilarsL1 = Set(rStarr.spouseValues: _*)
    val strategyDirected = SearchDirectedL2Strategy(rStarr.spouseProp, rStarr.spouseValues)
    val expectedSimilarsDirected = strategyDirected.findSimilars().keySet
    val strategyUndirected = SearchUndirectedL2Strategy(rStarr.spouseProp, rStarr.spouseValues)
    val actualSimilars = strategyUndirected.findSimilars().keySet
    assert(expectedSimilarsL1.size + expectedSimilarsDirected.size < actualSimilars.size)
  }
  test("Search undirected timeout should not kill", ActiveSlowTag) {
    val strategyUndirected = SearchUndirectedL2Strategy(rStarr.countryOfCitizenShipProperty, List(rStarr.countryOfCitizenShipValue))
    val similars = strategyUndirected.findSimilars()
    assert(similars.keySet.contains(SimilarPropertyOntology.w.toString + SimilarPropertyOntology.timeoutElement))
  }
  test("BFS search for ringo starr should work") {
    val simFinder = new SimilarityFinder2(rStarr.id)
    val simEntities = simFinder.findInitialEntitiesAsSet()
    println(simEntities.take(100))
    assert(simEntities.size > 100)
    println(s"# entities found: ${simEntities.size}")
  }


}
