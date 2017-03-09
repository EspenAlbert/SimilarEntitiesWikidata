package similarityFinder.ranker

import core.feature.Feature
import core.rdf.GraphRDF

import scala.collection.mutable.ListBuffer

/**
  * Created by Espen on 09.11.2016.
  */
object Ranker {

  def getSortedOrder(comparators : Map[String, ListBuffer[Feature]]) : List[SimilarEntity] = {
    val similarEntities = for(key <- comparators.keys) yield new SimilarEntity(key, comparators(key).toList)
    try {
      return similarEntities.toList.sorted
    } catch {
      case e : Throwable => println("failed to sort similar entities list"); return similarEntities.toList
    }
  }
  def getSortedOrderScaled(comparators: Map[String, ListBuffer[Feature]], otherEntitiesAsGraphs: List[GraphRDF], queryEntityGraph : GraphRDF): List[SimilarEntity] = {
      val similarEntities = for (
        graph <- otherEntitiesAsGraphs;
        scalingFactor = queryEntityGraph.findScalingFactor(graph);
        otherEntity = graph.entity;
        similarEntity = new SimilarEntity(otherEntity, comparators(otherEntity).toList, scalingFactor)
      ) yield similarEntity
    try {
      return similarEntities.sorted
    } catch {
      case e : Throwable => println("failed to sort similar entities list "); return similarEntities
    }
  }
}
