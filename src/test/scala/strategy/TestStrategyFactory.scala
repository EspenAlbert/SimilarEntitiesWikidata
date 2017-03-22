package strategy

import core.globals.KnowledgeGraph
import core.strategies.{PropMatchStrategy, StrategyFactory}
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.ActiveSlowTag

/**
  * Created by espen on 22.03.17.
  */
class TestStrategyFactory extends FunSuite{
  test("All strategies in Wikidata should be found and created a correct strategies for", ActiveSlowTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val sFactory = new StrategyFactory()
    val b = sFactory.mapPropertyToStrategies
    val c = sFactory.mapPropertyToStrategies
    assert(b == c)
    val directLinkStrategy = b.getOrElse(WikidataFactory.directLinkProperty, throw new Exception("fail"))
    assert(directLinkStrategy.length > 1)
    val valueMatchObjectStrategy = b.getOrElse(WikidataFactory.valueMatchObject, throw new Exception("fail"))
    assert(valueMatchObjectStrategy.length > 1)
    val propertyMatchStrategy = b.getOrElse(WikidataFactory.itemProperty, throw new Exception("fail"))
    assert(propertyMatchStrategy.length == 1)
    val obama = WikidataFactory.obama
    val rdfType = List(WikidataFactory.human)
//    val propertyMatchStrategiesActual = StrategyFactory.getStrategies(obama, rdfType, WikidataFactory.itemProperty, true, )


  }

}
