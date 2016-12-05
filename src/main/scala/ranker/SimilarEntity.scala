package ranker

import breeze.linalg.sum
import breeze.numerics._
import feature.Feature

/**
  * Created by Espen on 11.11.2016.
  */
class SimilarEntity(val name : String, features : List[Feature], scalingFactor : Double = 1.0) extends Ordered[SimilarEntity]{

  val sortedFeatures = try {
     features.sorted
  } catch {
    case e : Throwable => println(s"failed to sort for $name", features, e); features
  }
  val score = sum(for(f <- features)yield f.getScore()) * scalingFactor

  override def compare(that: SimilarEntity): Int = {
    val compared = floor(that.score - this.score).toInt
    try {
      assert(compared.isInstanceOf[Int])
      return compared
    } catch {
      case e: Throwable => println(s"failed to compare two similar entities with feature lists $sortedFeatures, ${that.sortedFeatures}"); return -1
    }
  }
}
