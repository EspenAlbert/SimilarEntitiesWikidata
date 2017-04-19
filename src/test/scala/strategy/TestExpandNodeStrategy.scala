package strategy

import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import core.strategies.{AggregatorStrategy, ExpandNodeStrategy, StrategyFactory}
import data.WikidataFactory
import org.scalatest.FunSuite
import similarityFinder.{MyConfiguration, SimilarityFinder2}
import tags.ActiveSlowTag

/**
  * Created by espen on 05.04.17.
  */
class TestExpandNodeStrategy extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val rStarr = WikidataFactory.ringoStarr
  val wd = WikidataFactory
  test("Should work for country of citizenship, it should not find many entities or timeout...", ActiveSlowTag) {
    StrategyFactory.setupStrategyFactory(List(ExpandNodeStrategy.name))
    val actualStrategiesBack = StrategyFactory.getStrategies(rStarr.id, rStarr.rdfTypes, rStarr.countryOfCitizenShipProperty, true, List(rStarr.countryOfCitizenShipValue))
    assert(actualStrategiesBack.forall(_.isInstanceOf[ExpandNodeStrategy]))
    val similarsFound = actualStrategiesBack.head.findSimilars()
    val anExpectedSimilar = wd.davidCameron
    val unexpectedSimilar = wd.johnLennon
    assert(similarsFound.contains(anExpectedSimilar))
    assert(!similarsFound.contains(unexpectedSimilar))
  }
  test("Similarity generation for rStarr", ActiveSlowTag) {
    StrategyFactory.setupStrategyFactory(List(ExpandNodeStrategy.name))
    val simFinder = new SimilarityFinder2(rStarr.id, useFilteringGraphRDF = true)
    val foundEntities = simFinder.findInitialEntitiesAsSet()
    assert(foundEntities.size > 100)
  }
  test("Similarity generation with filtering for rStarr", ActiveSlowTag) {
    MyConfiguration.filterOnRdfType = false
    StrategyFactory.setupStrategyFactory(List(ExpandNodeStrategy.name))
    val simFinder = new SimilarityFinder2(rStarr.id, useFilteringGraphRDF = true)
    val foundEntities = simFinder.findInitialEntitiesAsSet()
    assert(foundEntities.size > 100)
    MyConfiguration.filterOnRdfType = true
    StrategyFactory.setupStrategyFactory(List(ExpandNodeStrategy.name))
    val simFinderWithFilter = new SimilarityFinder2(rStarr.id, useFilteringGraphRDF = true)
    val foundEntitiesWithFilter = simFinderWithFilter.findInitialEntitiesAsSet()
    assert(foundEntitiesWithFilter.size > 100 && foundEntitiesWithFilter.size < foundEntities.size)
  }
  test("Similarity generation with useMustHaveProperty for rStarr", ActiveSlowTag) {
    MyConfiguration.filterOnRdfType = false
    StrategyFactory.setupStrategyFactory(List(ExpandNodeStrategy.name))
    val simFinder = new SimilarityFinder2(rStarr.id, useFilteringGraphRDF = true)
    val foundEntities = simFinder.findInitialEntitiesAsSet()
    assert(foundEntities.size > 100)
    MyConfiguration.useMustHaveProperty = true
    StrategyFactory.setupStrategyFactory(List(ExpandNodeStrategy.name))
    val simFinderWithFilter = new SimilarityFinder2(rStarr.id, useFilteringGraphRDF = true)
    val foundEntitiesWithFilter = simFinderWithFilter.findInitialEntitiesAsSet()
    assert(foundEntitiesWithFilter.size > 100 && foundEntitiesWithFilter.size < foundEntities.size)
  }
}
