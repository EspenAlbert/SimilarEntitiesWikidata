package similarityFinder

import core.globals.{KnowledgeGraph, ResultsSimilarArtistsGlobals}
import core.strategies.{DirectLinkStrategy, Strategy, ValueMatchStrategy}
import org.scalatest.FunSuite
import similarityFinder.RunName._
import tags.ActiveTag
/**
  * Created by espen on 30.03.17.
  */
class TestRunName extends FunSuite{
  test("A correct name for a run should be created", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val dlStrategy = DirectLinkStrategy(null, Set())
    val vmStrategy = ValueMatchStrategy(null, true,null, Nil, 0)
    val actual = getRunName(List[Strategy](dlStrategy))
    val expected = ResultsSimilarArtistsGlobals.base + knowledgeGraph + "-DirectLinkStrategy-"
    assert(actual == expected)
    val actual2 = getRunName(List[Strategy](vmStrategy, dlStrategy))
    val expected2 = ResultsSimilarArtistsGlobals.base + knowledgeGraph + "-DirectLinkStrategy-ValueMatchStrategy-"
    assert(actual2 == expected2)
  }

}
