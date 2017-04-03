package similarityFinder

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.ResultsSimilarArtistsGlobals
import core.strategies._

/**
  * Created by espen on 30.03.17.
  */
object RunName {

  def getStrategyOrder(strategy: String) = strategy match {
    case DirectLinkStrategy.name => 1
    case ValueMatchStrategy.name => 2
    case PropertyMatchStrategy.name => 3
    case DateComparisonStrategy.name => 4
    case SearchDirectedL1Strategy.name => 5
    case SearchDirectedL2Strategy.name => 6
    case SearchUndirectedL1Strategy.name => 7
    case SearchUndirectedL2Strategy.name => 8
  }

  def getRunName(strategies : List[String])(implicit knowledgeGraph: KnowledgeGraph): String = {
    val strategiesInSortedOrder = strategies.map(s => (getStrategyOrder(s), s)).sortBy[Int](_._1)
    val name : String = knowledgeGraph + "-" +  strategiesInSortedOrder.map(_._2).mkString("-")
    return ResultsSimilarArtistsGlobals.base + name + "-" + MyConfiguration.getConfigName
  }

}
