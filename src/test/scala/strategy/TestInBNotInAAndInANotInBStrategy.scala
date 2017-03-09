package strategy

import core.rdf.GraphRDF
import org.scalatest.FunSuite
import core.strategies._
import similarityFinder.MyConfiguration

/**
  * Created by Espen on 11.11.2016.
  */
class TestInBNotInAAndInANotInBStrategy extends FunSuite{
  test("Sorting the core.strategies should work") {
    val st1Count = MyConfiguration.inBNotInABoost * MasterStrategy.logarithmicWeightForCount(2)
    val st2Count = MyConfiguration.inBNotInABoost * MasterStrategy.logarithmicWeightForCount(5)
    val strategy = InBNotInAStrategy("http://www.wikidata.org/entity/P21",true, List("http://www.wikidata.org/entity/Q1052281"),st1Count)
    val strategy2 = InBNotInAStrategy("http://www.wikidata.org/entity/P21",true, List("http://www.wikidata.org/entity/Q6581097"),st2Count)

    val list = List[Strategy](strategy, strategy2)
    val sortedList = list.sorted
    assert(sortedList(0) == strategy2)//Woops, remember they are negative...
    assert(sortedList(1) == strategy)
    assert(strategy.findSimilars() == Nil)
  }
  test("Creating the inBNotInA strategy from master strategy for a transgender female, then checking that obama will get a feature in BnotInA") {
    val strategies = MasterStrategy.matchStrategyClassNameToStrategy("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchObjectStrategy", "http://www.wikidata.org/entity/P21",
      domain = List(), range = List("http://www.wikidata.org/entity/Q1052281"), rdfType ="http://www.wikidata.org/entity/Q5", entity= "http://www.wikidata.org/entity/Q16489")
    val inBNotInAStrategyList = strategies match {
      case Some(a) => println(a); a.filter(_.isInstanceOf[InBNotInAStrategy])
      case None => println("Failed to find core.strategies"); assert(false); List()
    }
    assert(inBNotInAStrategyList.length == 1)
    val strategy = inBNotInAStrategyList(0)
    val featureMap = strategy.execute(List(new GraphRDF("w:Q76")))
    println(featureMap)
    assert(featureMap.size == 1)
  }
  test("Creating the inANotInB strategy from master strategy for a transgender female, then checking that obama will get a feature inANotInB") {
    val strategies = MasterStrategy.matchStrategyClassNameToStrategy("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchObjectStrategy", "http://www.wikidata.org/entity/P21",
      domain = List(), range = List("http://www.wikidata.org/entity/Q1052281"), rdfType ="http://www.wikidata.org/entity/Q5", entity= "http://www.wikidata.org/entity/Q16489")
    val inANotInBStrategyList = strategies match {
      case Some(a) => println(a); a.filter(_.isInstanceOf[InANotInBStrategy])
      case None => println("Failed to find core.strategies"); assert(false); List()
    }
    assert(inANotInBStrategyList.length == 1)
    val strategy = inANotInBStrategyList(0)
    val featureMap = strategy.execute(List(new GraphRDF("w:Q76")))
    println(featureMap)
    assert(featureMap.size == 1)
  }
  test("Properties that hillary has but obama hasn't should count negatively for their similarity") {
    val obama = new GraphRDF("w:Q76")
    val hillary = new GraphRDF("w:Q6294")
    val strategies = StrategyGenerator.generateStrategies(obama)
    val inBNotInAGlobalStrategy = strategies.filter(_.isInstanceOf[InBNotInAGlobalStrategy])
    assert(inBNotInAGlobalStrategy.length == 1)
    val featureMap = inBNotInAGlobalStrategy(0).execute(List(hillary))
    println(featureMap)
  }
  test("Properties that obama has but hillary hasn't should count negatively for their similarity") {
    val obama = new GraphRDF("w:Q76")
    val hillary = new GraphRDF("http://www.wikidata.org/entity/Q6294")
    val strategies = StrategyGenerator.generateStrategies(obama)
    val inBNotInAGlobalStrategy = strategies.filter(_.isInstanceOf[InANotInBStrategy])
    assert(inBNotInAGlobalStrategy.length > 0)
    val featureMap = inBNotInAGlobalStrategy.map(_.execute(List(hillary)))
    println(featureMap.toList)
  }
  test("Obama should get no in a not in b or otherwise for himself") {
    val obama = new GraphRDF("w:Q76")
    val hillary = new GraphRDF("http://www.wikidata.org/entity/Q6294")
    val strategies = StrategyGenerator.generateStrategies(obama)
    val inBNotInAGlobalStrategy = strategies.filter(_.isInstanceOf[InANotInBStrategy])
    assert(inBNotInAGlobalStrategy.length > 0)
    val featureMap = inBNotInAGlobalStrategy.map(_.execute(List(obama)))
    println(featureMap.toList)
    val fMapFromInA = strategies.filter(_.isInstanceOf)
  }
}
