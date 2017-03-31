package strategy

import core.globals.KnowledgeGraph
import core.strategies.{SearchDirectedL1Strategy, SearchDirectedL2Strategy, SearchUndirectedL1Strategy, SearchUndirectedL2Strategy}
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.ActiveTag

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

}
