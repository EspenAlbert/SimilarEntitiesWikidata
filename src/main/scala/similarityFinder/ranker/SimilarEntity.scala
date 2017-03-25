package similarityFinder.ranker

import breeze.linalg.sum
import breeze.numerics._
import core.feature.Feature

/**
  * Created by Espen on 11.11.2016.
  */
class SimilarEntity(val name : String, features : List[Feature], scalingFactor : Double = 1.0) extends Ordered[SimilarEntity]{

  val sortedFeatures = features.sorted
  val score = sum(for(f <- features)yield f.getScore()) * scalingFactor

  override def compare(that: SimilarEntity): Int = {
    val compared = that.score - this.score
    if(compared > 0) return 1 //Other has higher score
    if(compared < 0) return -1 //This has higher score
    return 0
  }
}
object SimilarEntity {
  def combine(sEntity: SimilarEntity, otherSimilarEntity: SimilarEntity): SimilarEntity = {
    println(s"combining ${sEntity.name}")
    return new SimilarEntity(sEntity.name, sEntity.sortedFeatures ++ otherSimilarEntity.sortedFeatures)
  }

}