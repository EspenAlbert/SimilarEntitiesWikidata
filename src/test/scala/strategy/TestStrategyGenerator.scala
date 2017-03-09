package strategy

import core.rdf.GraphRDF
import org.scalatest.FunSuite
import core.strategies._

/**
  * Created by Espen on 11.11.2016.
  */
class TestStrategyGenerator extends FunSuite{
  test("The correct core.strategies for obama are created") {
    val strategies = StrategyGenerator.generateStrategies(new GraphRDF("w:Q76"))
    print(strategies)
    assert(strategies.length == 2)
  }
  test("Direct link strategy is created for the relative property") {
    val property = "http://www.wikidata.org/entity/P1038"
    val strategyStrings = StrategyFactory.getStrategies(property)
    assert(strategyStrings.exists(_.contains("DirectLinkStrategy")))
//    val core.strategies = strategyStrings.map(MasterStrategy.matchStrategyClassNameToStrategy(_, property, ))
//    MasterStrategy.matchStrategyClassNameToStrategy()
  }
  test("Direct link core.strategies for obama") {
    val strategies = StrategyGenerator.generateStrategies(new GraphRDF("w:Q76"))
    val directLinkStrategies = strategies.filter(_.isInstanceOf[DirectLinkStrategy])
    println(directLinkStrategies)
    assert(directLinkStrategies.length == 10)
    val someOtherEntities = List(new GraphRDF("http://www.wikidata.org/entity/Q4115068"), new GraphRDF("http://www.wikidata.org/entity/Q15982322"))
    for(s <- directLinkStrategies) {
      println(s.findSimilars())
      println(s.execute(someOtherEntities))
      println(s.weight)
    }
  }
  test("Alternative link strategy is created for the relative property") {
    val property = "http://www.wikidata.org/entity/P1038"
    val strategyStrings = StrategyFactory.getStrategies(property)
    assert(strategyStrings.exists(_.contains("AlternativeLinkStrategy")))
    //    val core.strategies = strategyStrings.map(MasterStrategy.matchStrategyClassNameToStrategy(_, property, ))
    //    MasterStrategy.matchStrategyClassNameToStrategy()
  }
  test("Alternative link core.strategies for obama") {
    val strategies = StrategyGenerator.generateStrategies(new GraphRDF("w:Q76"))
    val alternativeLinkStrategies = strategies.filter(_.isInstanceOf[AlternativeLinkStrategy])
    println(alternativeLinkStrategies)
    assert(alternativeLinkStrategies.length == 10)
    val someOtherEntities = List(new GraphRDF("http://www.wikidata.org/entity/Q4115068"), new GraphRDF("http://www.wikidata.org/entity/Q15982322"))
    for(s <- alternativeLinkStrategies) {
      println(s.execute(someOtherEntities))
      println(s.weight)
    }
  }
}
