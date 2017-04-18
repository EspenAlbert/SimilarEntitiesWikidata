package preprocessing.ownOntologyPopularizer

import core.globals.KnowledgeGraph
import core.query.specific.{QueryFactory, UpdateQueryFactory}
import core.strategies.StrategyFactory
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.{ActiveOnceTag, ActiveTag}

/**
  * Created by espen on 18.04.17.
  */
class TestIsDescriptiveClassifier extends FunSuite{

  test("Should classify wikidata properties correctly", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val wd = WikidataFactory
    val descriptiveProperties = List(wd.ringoStarr.countryOfCitizenShipProperty, wd.ringoStarr.genderProp, wd.ringoStarr.performerProp, wd.ringoStarr.memberOfProp)
    val entityConnectingProperties = List(wd.ringoStarr.spouseProp)
    val actual = IsDescriptivePropertyClassifier.classify(descriptiveProperties)
    val expected = List.fill(descriptiveProperties.size)(true)
    assert(actual == expected)
    val expectedEntityConnectingProps = List.fill(entityConnectingProperties.size)(false)
    val actualEConnecting = IsDescriptivePropertyClassifier.classify(entityConnectingProperties)
    assert(actualEConnecting == expectedEntityConnectingProps)
  }
  test("Full generation wikidata should work", ActiveOnceTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val (properties, _) = QueryFactory.findAllDomainCounts()
    val booleans = IsDescriptivePropertyClassifier.classify(properties)
    for{
      (p, isD) <- properties.zip(booleans)
    }{
      UpdateQueryFactory.addIsDescriptive(p, isD)
    }
    }
  test("Generated from strategyfactory should work", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    StrategyFactory.forceRead = true
    val wd = WikidataFactory
    val descriptiveProperties = List(wd.ringoStarr.countryOfCitizenShipProperty, wd.ringoStarr.genderProp, wd.ringoStarr.performerProp, wd.ringoStarr.memberOfProp)
    val entityConnectingProperties = List(wd.ringoStarr.spouseProp)
    val sFactory = new StrategyFactory()
    assert(descriptiveProperties.forall(p => sFactory.mapPropertyToIsDescriptive(p)))
    assert(entityConnectingProperties.forall(p => !sFactory.mapPropertyToIsDescriptive(p)))
  }

}
