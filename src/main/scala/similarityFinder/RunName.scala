package similarityFinder

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.ResultsSimilarArtistsGlobals
import core.strategies._

/**
  * Created by espen on 30.03.17.
  */
object RunName {

  def getStrategyOrder(strategy: Strategy) = strategy match {
    case a: DirectLinkStrategy => 1
    case b: ValueMatchStrategy => 2
    case c: PropertyMatchStrategy => 3
    case b: DateComparisonStrategy => 4
  }

  def getRunName(strategies : List[Strategy])(implicit knowledgeGraph: KnowledgeGraph): String = {
    val strategiesInSortedOrder = strategies.map(s => (getStrategyOrder(s), s)).sortBy[Int](_._1)
    val name : String = knowledgeGraph + "-" +  strategiesInSortedOrder.map(_._2.name).mkString("-")
    return ResultsSimilarArtistsGlobals.base + name + "-" + MyConfiguration.getConfigName
  }

}
