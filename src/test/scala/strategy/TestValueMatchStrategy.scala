package strategy

import globals.MyConfiguration
import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.AttributeGenerator
import rdf.GraphRDF
import strategies.{MasterStrategy, PropMatchStrategy, Strategy, ValueMatchStrategy}

/**
  * Created by Espen on 11.11.2016.
  */
class TestValueMatchStrategy extends FunSuite{
  test("Value match strategy should work") {
    val st1Count = MyConfiguration.valueMatchBoost * MasterStrategy.logarithmicWeightForCount(2)
    val st2Count = MyConfiguration.valueMatchBoost * MasterStrategy.logarithmicWeightForCount(5)
    val strategy = ValueMatchStrategy("http://www.wikidata.org/entity/P43",true, "http://www.wikidata.org/entity/Q43274", "http://www.wikidata.org/entity/Q5", st1Count)
    val strategy2 = ValueMatchStrategy("http://www.wikidata.org/entity/P1038", true, "http://www.wikidata.org/entity/Q10479","http://www.wikidata.org/entity/Q5", st2Count)

    val list = List[Strategy](strategy, strategy2)
    val sortedList = list.sorted
    assert(sortedList(0) == strategy)
    assert(sortedList(1) == strategy2)
    println(strategy.findSimilars())
    println(strategy2.findSimilars())
  }
  test("Creating the value match strategy from master strategy") {
    val strategies = MasterStrategy.matchStrategyClassNameToStrategy("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchStrategy", "http://www.wikidata.org/entity/P43",
      domain = List(), range = List("http://www.wikidata.org/entity/Q43274","http://www.wikidata.org/entity/Q3743314"), rdfType ="http://www.wikidata.org/entity/Q5", entity= "Do not matter")
    strategies match {
      case Some(a) => println(a); assert(true)
      case None => println("Failed to find strategies"); assert(false)
    }
  }
  test("Check that the execution runs correctly") {
    val graph1 = new GraphRDF("w:Q3736070")
    val graph2 = new GraphRDF("w:Q3743314")
    val st1Count = MyConfiguration.valueMatchBoost * MasterStrategy.logarithmicWeightForCount(2)
    val strategy = ValueMatchStrategy("http://www.wikidata.org/entity/P43",true, "http://www.wikidata.org/entity/Q43274", "http://www.wikidata.org/entity/Q5", st1Count)
    assert(strategy.execute(List(graph1, graph2)).toList.length == 2)
  }
  test("Transgender count Q1052281") {
    MasterStrategy.valueIsAPotentialValueMatchFindCount("http://www.wikidata.org/entity/Q1052281", "http://www.wikidata.org/entity/P21", true) match {
      case Some(c) => assert(c > 1); println(c)
      case None => assert(false)
    }
  }
  test("Male count Q6581097") {
    MasterStrategy.valueIsAPotentialValueMatchFindCount("http://www.wikidata.org/entity/Q6581097", "http://www.wikidata.org/entity/P21", true) match {
      case Some(c) => assert(c > 1); println(c)
      case None => assert(false)
    }
  }
}
