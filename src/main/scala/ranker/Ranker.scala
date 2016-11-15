package ranker

import feature.Feature

/**
  * Created by Espen on 09.11.2016.
  */
object Ranker {
  def getSortedOrder(comparators : Map[String, List[Feature]]) : List[SimilarEntity] = {
    val similarEntities = for(key <- comparators.keys) yield new SimilarEntity(key, comparators(key))
    return similarEntities.toList.sorted
  }

}
