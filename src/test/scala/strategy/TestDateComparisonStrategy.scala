package strategy

import displayer.Displayer
import feature.Feature
import globals.MyConfiguration
import org.scalatest.FunSuite
import query.specific.QueryFactoryV2
import query.variables.OptionsForResultQueryVariable
import ranker.Ranker
import rdf.{GraphRDF, SimpleRDFFactory}
import strategies.{DateComparisonStrategy, MasterStrategy, Strategy, ValueMatchStrategy}

import scala.collection.mutable.ListBuffer

/**
  * Created by Espen on 11.11.2016.
  */
class TestDateComparisonStrategy extends FunSuite{
  test("DateComparison should work") {
    val st1Count = MyConfiguration.dateComparisonWeight * MasterStrategy.logarithmicWeightForCount(2)
    val st2Count = MyConfiguration.dateComparisonWeight * MasterStrategy.logarithmicWeightForCount(5)
    val strategy = DateComparisonStrategy("http://www.wikidata.org/entity/P2031", "\"2011\"^^xsd:gYear", st1Count)
    val strategy2 = DateComparisonStrategy("http://www.wikidata.org/entity/P2031", "\"2011\"^^xsd:gYear", st2Count)
    val list = List[Strategy](strategy2, strategy)
    val sortedList = list.sorted
    assert(sortedList(0) == strategy)
    assert(sortedList(1) == strategy2)
    println(strategy.findSimilars())
    println(strategy2.findSimilars())
  }
  test("Converting to a standard format works as expected") {
    val differentDateValues : List[String]= QueryFactoryV2.findList(SimpleRDFFactory.getStatement("?s " + OptionsForResultQueryVariable.ignoreMe, "w:P1319", "?o"))
    println(differentDateValues)
    for(a <- differentDateValues) {
      println(DateComparisonStrategy.getStandardDateFormat(a))
    }
//    val strategies = MasterStrategy.matchStrategyClassNameToStrategy("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchStrategy", "http://www.wikidata.org/entity/P43",
//      domain = List(), range = List("http://www.wikidata.org/entity/Q43274","http://www.wikidata.org/entity/Q3743314"), rdfType ="http://www.wikidata.org/entity/Q5", entity= "Do not matter")
//    strategies match {
//      case Some(a) => println(a); assert(true)
//      case None => println("Failed to find strategies"); assert(false)
//    }
  }
  test("Check that the execution runs correctly") {
    val graph1 = new GraphRDF("w:Q436113")
    val graph2 = new GraphRDF("w:Q3371101")
    val graph3 = new GraphRDF("w:Q254")
    val st1Count = MyConfiguration.valueMatchBoost * MasterStrategy.logarithmicWeightForCount(2)
    val strategy = DateComparisonStrategy("http://www.wikidata.org/entity/P569","\"1970\"^^<http://www.w3.org/2001/XMLSchema#gYear>", st1Count)
    val features = strategy.execute(List(graph1, graph2, graph3))
    assert(features.toList.length == 3)
    println(features.values)
    val map = features.map{s : (String, Feature) => s._1 -> ListBuffer(s._2)}
    val ranked = Ranker.getSortedOrder(map)
    println(ranked)
  }
}
