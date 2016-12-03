package ranker

import feature.Feature

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
      case e : Throwable => println("failed to sort ", similarEntities.toList); return similarEntities.toList
    }
  }

}
