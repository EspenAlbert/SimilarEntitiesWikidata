package strategy

import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import org.scalatest.FunSuite
import core.strategies.{MasterStrategy, PropertyMatchStrategy, Strategy, ValueMatchStrategy}
import similarityFinder.MyConfiguration

/**
  * Created by Espen on 11.11.2016.
  */
class TestValueMatchStrategy extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("Value match strategy should work") {
    val strategy = ValueMatchStrategy("http://www.wikidata.org/entity/P43",true, "http://www.wikidata.org/entity/Q43274", List("http://www.wikidata.org/entity/Q5"), 5)
    val strategy2 = ValueMatchStrategy("http://www.wikidata.org/entity/P1038", true, "http://www.wikidata.org/entity/Q10479",List("http://www.wikidata.org/entity/Q5"), 55)

    val list = List[Strategy](strategy, strategy2)
    val sortedList = list.sorted
    assert(sortedList(0) == strategy)
    assert(sortedList(1) == strategy2)
    println(strategy.findSimilars())
    println(strategy2.findSimilars())
  }
  test("Creating the value match strategy from master strategy") {
    val strategies = MasterStrategy.matchStrategyClassNameToStrategy("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchStrategy", "http://www.wikidata.org/entity/P43",
      domain = List(), range = List("http://www.wikidata.org/entity/Q43274","http://www.wikidata.org/entity/Q3743314"), rdfTypes =List("http://www.wikidata.org/entity/Q5"), entity= "Do not matter")
    strategies match {
      case Some(a) => println(a); assert(true)
      case None => println("Failed to find core.strategies"); assert(false)
    }
  }
  test("Check that the execution runs correctly") {
    val graph1 = new GraphRDF("w:Q3736070")
    val graph2 = new GraphRDF("w:Q3743314")
    val strategy = ValueMatchStrategy("http://www.wikidata.org/entity/P43",true, "http://www.wikidata.org/entity/Q43274", List("http://www.wikidata.org/entity/Q5"), 5)
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
