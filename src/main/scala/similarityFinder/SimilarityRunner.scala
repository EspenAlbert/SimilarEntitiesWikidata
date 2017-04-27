package similarityFinder

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.strategies.{ExpandNodeStrategy, SearchUndirectedL2Strategy, StrategyFactory}

import scala.collection.parallel.mutable.ParHashMap

/**
  * Created by espen on 27.04.17.
  */
object SimilarityRunner {

  def main(args: Array[String]): Unit = {
    val testOriginal =  ParHashMap(("d", 1)) ++ParHashMap(("ab", 2), ("bc",3), ("d", 8))
    println(testOriginal)
    println(findTopKOfLength(testOriginal, 2))
  }


  def findTopTenMostSimilarToUsingBFS2(entity : String, strategyName : String = ExpandNodeStrategy.name)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    StrategyFactory.setupStrategyFactory(List(strategyName))(knowledgeGraph)
    val initialSimilarsWithFeatures = new SimilarityFinder2(entity).findInitialEntitiesAsMap()
    val initialSimilarsWithFeaturesLengths =initialSimilarsWithFeatures.map(pair => pair._1 -> pair._2.length)
    return findTopKOfLength(initialSimilarsWithFeaturesLengths, 10)

  }

  def findTopKOfLength(initialSimilarsWithFeaturesLengths: ParHashMap[String, Int], currentSimilar : Int): List[String] = {
    currentSimilar match {
      case 0 => Nil
      case a =>
        val topSimilar = initialSimilarsWithFeaturesLengths.maxBy(_._2)._1
        initialSimilarsWithFeaturesLengths.update(topSimilar, 0)
        topSimilar :: findTopKOfLength(initialSimilarsWithFeaturesLengths, currentSimilar -1)
    }

  }

}
