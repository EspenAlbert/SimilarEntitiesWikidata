package structureFinder.pathsBetweenEntities

import core.globals.KnowledgeGraph
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.strategies.StrategyFactory
import structureFinder.KConnectivitySparqlBuilder.Path

/**
  * Created by espen on 28.04.17.
  */
object PathRanker {

  def main(args: Array[String]): Unit = {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    createPropertyRankMap
  }

  def createPropertyRankMap(implicit knowledgeGraph : KnowledgeGraph) : Map[String, Double] = {
    val sFactory = new StrategyFactory()
    val domainCounts = sFactory.mapPropertyToDomainCounts
    val maxCount = domainCounts.values.max
    val propertyScoredByRareness = domainCounts.map{
      case (key, value) => key -> (1.toDouble - value / maxCount)
    }
    assert(propertyScoredByRareness.values.min == 0)
    assert(propertyScoredByRareness.size == domainCounts.size)
    return propertyScoredByRareness
  }

  def rankOnPropertyRareness(paths : Iterable[Path], topk : Int)(implicit knowledgeGraph : KnowledgeGraph) : List[Path] = {
    val propertyScoreMap = createPropertyRankMap.withDefaultValue(0.0)
    val pathsWithScore = paths.map(p => {
      val propertySum = p.properties
        .map(prop => propertyScoreMap(KnowledgeGraph.getPropertyWithoutQualifierName(prop)))
        .sum
      (1 + (propertySum / p.properties.size), p)
    }  )
    val sorted = pathsWithScore.toList.sortBy(_._1)
    assert(sorted.size == 1 || sorted.head._1 <= sorted.last._1, s"some weird sorting: $sorted")
    sorted.takeRight(topk).map(_._2)
  }

}
