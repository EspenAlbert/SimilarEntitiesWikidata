package pruning

import core.globals.KnowledgeGraph
import org.scalatest.FunSuite
import similarityFinder.SimilarityFinder

/**
  * Created by espen on 03.02.17.
  */
class TestPruning extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("Result for Elton John") {
    val (graph, strategies) = SimilarityFinder.findGraphAndStrategiesForEntity("http://www.wikidata.org/entity/Q2808")
    val foundSimilarsAlgorithm = SimilarityFinder.findSimilarsUnranked(strategies)
//    val similars = SimilarityFinder.findTopKSimilarTo("w:Q2808", 10)
    val expectedSimilars = List("http://www.wikidata.org/entity/Q194333", "http://www.wikidata.org/entity/Q182655", "http://www.wikidata.org/entity/Q2599", "http://www.wikidata.org/entity/Q2632", "http://www.wikidata.org/entity/Q1225", "http://www.wikidata.org/entity/Q218255", "http://www.wikidata.org/entity/Q144622", "http://www.wikidata.org/entity/Q714", "http://www.wikidata.org/entity/Q272203", "http://www.wikidata.org/entity/Q1203")
//    val foundSimilars = expectedSimilars.filter((s) => similars.exists(_.name == s))

    val foundSimilars = expectedSimilars.filter(foundSimilarsAlgorithm.contains(_))
    println(foundSimilars.length)
    println("not found: ", expectedSimilars.filterNot(foundSimilars.contains(_)))
    println("found: ", foundSimilars)
    println("-----------WITH PRUNING---------")
    val foundSimilarsAlgorithmPruning = SimilarityFinder.findSimilarsUnrankedWithPruning(strategies)
    val foundSimilarsWithPruning = expectedSimilars.filter(foundSimilarsAlgorithmPruning.contains(_))
    println(foundSimilarsWithPruning.length)
    println("not found: ", expectedSimilars.filterNot(foundSimilarsWithPruning.contains(_)))
    println("found: ", foundSimilarsWithPruning)

  }

}
