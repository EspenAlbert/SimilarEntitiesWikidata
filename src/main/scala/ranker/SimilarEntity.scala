package ranker

import breeze.linalg.sum
import breeze.numerics._
import feature.Feature

/**
  * Created by Espen on 11.11.2016.
  */
class SimilarEntity(val name : String, features : List[Feature]) extends Ordered[SimilarEntity]{
  val sortedFeatures = features.sorted
  val score = sum(for(f <- features)yield f.getScore())

  override def compare(that: SimilarEntity): Int = {
    return floor(that.score - this.score).toInt
  }
}
