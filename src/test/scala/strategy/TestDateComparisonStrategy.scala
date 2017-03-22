package strategy

import core.feature.Feature
import core.globals.{KnowledgeGraph, PrimitiveDatatype}
import core.rdf.GraphRDF
import org.scalatest.FunSuite
import similarityFinder.ranker.Ranker
import core.strategies.{DateComparisonStrategy, MasterStrategy, Strategy, ValueMatchStrategy}
import similarityFinder.MyConfiguration

import scala.collection.mutable.ListBuffer

/**
  * Created by Espen on 11.11.2016.
  */
class TestDateComparisonStrategy extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("DateComparison should work") {
    val strategy = DateComparisonStrategy("http://www.wikidata.org/entity/P2031", "\"2011\"^^xsd:gYear", 2)
    val strategy2 = DateComparisonStrategy("http://www.wikidata.org/entity/P2031", "\"2011\"^^xsd:gYear", 5)
    val list = List[Strategy](strategy2, strategy)
    val sortedList = list.sorted
    assert(sortedList(0) == strategy)
    assert(sortedList(1) == strategy2)
    println(strategy.findSimilars())
    println(strategy2.findSimilars())
  }
  test("Check that the execution runs correctly") {
    val graph1 = new GraphRDF("w:Q436113")
    val graph2 = new GraphRDF("w:Q3371101")
    val graph3 = new GraphRDF("w:Q254")
    val strategy = DateComparisonStrategy("http://www.wikidata.org/entity/P569","\"1970\"^^<http://www.w3.org/2001/XMLSchema#gYear>", 2)
    val features = strategy.execute(List(graph1, graph2, graph3))
    assert(features.toList.length == 3)
    println(features.values)
    val map = features.map{s : (String, Feature) => s._1 -> ListBuffer(s._2)}
    val ranked = Ranker.getSortedOrder(map)
    println(ranked)
  }
}
