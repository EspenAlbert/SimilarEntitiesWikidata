package similarityFinder

import core.globals.{KnowledgeGraph, SimilarPropertyOntology}
import core.strategies._
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.{ActiveSlowTag, ActiveTag}

/**
  * Created by espen on 03.04.17.
  */
class TestMyConfiguration extends FunSuite{

  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val ringoStarr = WikidataFactory.ringoStarr
  val wd = WikidataFactory
  test("John Lennon should be one of the similars found by VM strategy when myConfiguration useRdfTypes=true or useRdfTypes=false", ActiveTag) {
    MyConfiguration.useRdfType = false
    val vmStrategy = ValueMatchStrategy(ringoStarr.memberOfProp, true, rdfTypes = ringoStarr.rdfTypes, value=ringoStarr.memberOfValue, dbCount = 200)
    val actualWithoutRDFTypes = vmStrategy.findSimilars()
    assert(actualWithoutRDFTypes.keySet.contains(wd.johnLennon))
    MyConfiguration.useRdfType = true
    val actualWithRdfTypes = vmStrategy.findSimilars()
    assert(actualWithRdfTypes.keySet.contains(wd.johnLennon))
  }
  test("The beatles should be one of the similars when myConfiguration useRdfTypes=false", ActiveTag) {
    MyConfiguration.useRdfType = false
    val beatles = ringoStarr.memberOfValue
    implicit val strategyFactory = new StrategyFactory()
    val vmStrategy = StrategyFactory.matchStrategyClassNameToStrategy(
      strategy = SimilarPropertyOntology.directLinkStrategy,
      property = ringoStarr.memberOfProp,
      domain = Nil,
      range = List(beatles),
      entity = ringoStarr.id,
      rdfTypes = ringoStarr.rdfTypes
    ).get.find(_.isInstanceOf[DirectLinkStrategy]).get
    val actualWithoutRDFTypes = vmStrategy.findSimilars()
    assert(actualWithoutRDFTypes.keySet.contains(beatles))
    MyConfiguration.useRdfType = true
    assert(StrategyFactory.matchStrategyClassNameToStrategy(
      strategy = SimilarPropertyOntology.directLinkStrategy,
      property = ringoStarr.memberOfProp,
      domain = Nil,
      range = List(beatles),
      entity = ringoStarr.id,
      rdfTypes = ringoStarr.rdfTypes
    ).isEmpty)
  }
  test("Property match without rdf types should work", ActiveSlowTag) {
    MyConfiguration.useRdfType = false
    val vegetarian = ringoStarr.lifestyleValue
    val strategy = PropertyMatchStrategy(ringoStarr.lifestyleProp, false, ringoStarr.rdfTypes, 200)
    val actualSimilars = strategy.findSimilars()
    assert(actualSimilars.keySet.contains(vegetarian))
    MyConfiguration.useRdfType = true
    val actualSimilarsWithType = strategy.findSimilars()
    assert(!actualSimilarsWithType.keySet.contains(vegetarian))
  }
  test("The runName should be set when default configuration is not set", ActiveTag) {
    MyConfiguration.useRdfType = true
    assert(RunName.getRunName(List(DirectLinkStrategy.name)).endsWith("UseRdfType"))
  }
  test("The runName should reflect the count when a property or valuematch strategy is being used", ActiveTag) {
    MyConfiguration.useRdfType = true
    val valueMatch = List(ValueMatchStrategy.name)
    assert(RunName.getRunName(valueMatch).endsWith("UseRdfType-1000"))
    MyConfiguration.thresholdCountCheapStrategy = 3000
    assert(RunName.getRunName(valueMatch).endsWith("UseRdfType-3000"))
    MyConfiguration.thresholdCountCheapStrategy = 10000
    assert(RunName.getRunName(valueMatch).endsWith("UseRdfType-10000"))
    MyConfiguration.thresholdCountCheapStrategy = 1000
    val propertyMatch = List(PropertyMatchStrategy.name)
    assert(RunName.getRunName(propertyMatch).endsWith("UseRdfType-1000"))
    MyConfiguration.thresholdCountCheapStrategy = 3000
    assert(RunName.getRunName(propertyMatch).endsWith("UseRdfType-3000"))
    MyConfiguration.thresholdCountCheapStrategy = 10000
    assert(RunName.getRunName(propertyMatch).endsWith("UseRdfType-10000"))
    MyConfiguration.useRdfType = false
    MyConfiguration.thresholdCountCheapStrategy = 1000
    assert(RunName.getRunName(propertyMatch).endsWith("1000"))
    MyConfiguration.thresholdCountCheapStrategy = 3000
    assert(RunName.getRunName(propertyMatch).endsWith("3000"))
    MyConfiguration.thresholdCountCheapStrategy = 10000
    assert(RunName.getRunName(propertyMatch).endsWith("10000"))

  }

}
