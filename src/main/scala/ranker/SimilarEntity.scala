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
    val compared = floor(that.score - this.score).toInt
    try {
      assert(compared.isInstanceOf[Int])
      return compared
    } catch {
      case e: Throwable => println(s"failed too compare two similar entities with feature lists $sortedFeatures, ${that.sortedFeatures}"); return -1
    }
  }
}
