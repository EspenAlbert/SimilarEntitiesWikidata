package preprocessing.ownOntologyPopularizer

import core.globals._
import core.strategies.StrategyFactory
import data.DBpediaFactory
import iAndO.dump.DumpObject
import org.scalatest.FunSuite
import preprocessing.ownOntologyPopularizer.attributesGenerator.PropTypeToStrategyCreator._
import tags.{ActiveOnceTag, ActiveTag}

import scala.collection.mutable

/**
  * Created by espen on 21.02.17.
  */
class TestProptypeToStrategyGeneratorDBpedia extends FunSuite{

  implicit val knowledgeGraph = KnowledgeGraph.dbPedia

  test("findMetaPropertyKnowledge", ActiveTag) {
    val mapping = DBpediaFactory.expectedPropertyMapping
    val (propToDomainCount: mutable.Map[String, Int], propToRangeCount: mutable.Map[String, Int], sameTypePossibleProps: mutable.Set[String], sharableDomainProps: mutable.Set[String], sharableRangeProps: mutable.Set[String], dateTimeStrategies : mutable.Set[String]) = findMetaPropertyKnowledge(mapping)
    assert(propToDomainCount.size == mapping.size)
    assert(sameTypePossibleProps.forall(DBpediaFactory.sameTypesPossible.contains(_)))
    assert(propToRangeCount.keys.forall(DBpediaFactory.rangeCountsProps.contains(_)))
    assert(sharableDomainProps.forall(DBpediaFactory.sharableDomain.contains(_)))
    assert(sharableRangeProps.forall(DBpediaFactory.rangeCountsProps.contains(_)))
    assert(dateTimeStrategies.head == DBpediaFactory.dateProperty)
  }
  test("full generation", ActiveOnceTag) {
    val propToType = DumpObject.readJsonMapStringPropertyType(DBpediaFactory.propToTypeFilename)
    generateMetaStatementKnowledgeAndStrategiesForProperties(propToType)
    StrategyFactory.forceRead = true
    val dbPediaStrategyFactory = new StrategyFactory()
    propToType.keys.filterNot(KnowledgeGraph.getTypeProperty(knowledgeGraph) == _).foreach(prop => checkNumberOfStrategiesForProperty(prop, dbPediaStrategyFactory, 0))
  }
  test("full generation for a single property", ActiveTag) {
    val ringoStarr = DBpediaFactory.ringoStarr
    val directLinkAndBothValueMatches = ringoStarr.spouseProp
    generateMetaStatementKnowledgeAndStrategiesForProperties(Map(directLinkAndBothValueMatches -> ItemPropertyType()))
    StrategyFactory.forceRead = true
    val dbPediaStrategyFactory = new StrategyFactory()
    checkNumberOfStrategiesForProperty(directLinkAndBothValueMatches, dbPediaStrategyFactory, 2)
  }

  private def checkNumberOfStrategiesForProperty(directLinkAndBothValueMatches: String, dbPediaStrategyFactory: StrategyFactory, strategyCountGreaterThan : Int) = {
    val strategies = dbPediaStrategyFactory.mapPropertyToStrategies.getOrElse(directLinkAndBothValueMatches, throw new Exception(s"Couldn't find the strategy for ${directLinkAndBothValueMatches} created from the DB.."))
    assert(strategies.size > strategyCountGreaterThan)
  }
  test("Baseline strategy creation for all item properties dbPedia", ActiveOnceTag) {
    val propToType = DumpObject.readJsonMapStringPropertyType(DBpediaFactory.propToTypeFilename)
    val strategyURIs = List[String](SimilarPropertyOntology.searchDirectedL1Strategy, SimilarPropertyOntology.searchDirectedL2Strategy,
      SimilarPropertyOntology.searchUndirectedL1Strategy, SimilarPropertyOntology.searchUndirectedL2Strategy)
    strategyURIs.foreach(sURI => addStrategyForAllItemProperties(propToType, sURI))
  }
}
