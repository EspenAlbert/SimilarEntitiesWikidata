package ranker

import feature.Feature

import scala.collection.mutable.ListBuffer

/**
  * Created by Espen on 09.11.2016.
  */
object Ranker {
  def getSortedOrder(comparators : Map[String, ListBuffer[Feature]]) : List[SimilarEntity] = {
    val similarEntities = for(key <- comparators.keys) yield new SimilarEntity(key, comparators(key).toList)
    return similarEntities.toList.sorted
  }

}
