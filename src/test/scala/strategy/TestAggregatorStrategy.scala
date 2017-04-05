package strategy

import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import core.strategies.{AggregatorStrategy, StrategyFactory}
import data.WikidataFactory
import org.scalatest.FunSuite
import similarityFinder.SimilarityFinder2
import tags.{ActiveSlowTag, ActiveTag}

/**
  * Created by espen on 05.04.17.
  */
class TestAggregatorStrategy extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val rStarr = WikidataFactory.ringoStarr
  test("Should work for performed by for ringo starr", ActiveSlowTag) {
    val gRDF = new GraphRDF(rStarr.id)
    val domainPerformedBy = gRDF.statementsList.collect{case (s, p, o) if(p==rStarr.performerProp) => s}
    StrategyFactory.setupStrategyFactory(List(AggregatorStrategy.name))
    val actualStrategiesBack = StrategyFactory.getStrategies(rStarr.id, rStarr.rdfTypes, rStarr.performerProp, false, domainPerformedBy)
    assert(actualStrategiesBack.forall(_.isInstanceOf[AggregatorStrategy]))
    val similarsFound = actualStrategiesBack.flatMap(_.findSimilars())
    val expectedSimilars = rStarr.otherPerformersWithMoreThan2RockMusicPerformances
    expectedSimilars.foreach(similar => {
      assert(similarsFound.exists(_._1 == similar))
    })
  }
  test("Similarity generation for rStarr", ActiveSlowTag) {
    StrategyFactory.setupStrategyFactory(List(AggregatorStrategy.name))
    val simFinder = new SimilarityFinder2(rStarr.id)
    val foundEntities = simFinder.findInitialEntitiesAsSet()
    assert(foundEntities.size > 100)
  }
}
