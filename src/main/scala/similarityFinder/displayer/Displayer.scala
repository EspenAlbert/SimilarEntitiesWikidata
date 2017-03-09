package similarityFinder.displayer

import core.feature.Feature
import similarityFinder.ranker.SimilarEntity

/**
  * Created by Espen on 09.11.2016.
  */
object Displayer {
  def displayResult(result : List[SimilarEntity], topK : Int, entity : String) : Unit = {
    println(s"Most similar to $entity is: ")
    for((r, i) <- result.slice(0, topK).zipWithIndex) {
      val label = Feature.findLabel(r.name)
      println(s"Most similar #$i is: ${r.name} with label $label: with similarity score : ${r.score}")
      try {
        printFeatureInfo(r)
      } catch {
        case a : Throwable => Unit
      }
    }
  }

  private def printFeatureInfo(r: SimilarEntity) = {
    println(s"Most important feature was ${r.sortedFeatures(0)}")
    for(i <- 1 to 20) {
      println(s"${i}nd most important feature was ${r.sortedFeatures(i)}")

    }
    println(s"Minimum import feature was ${r.sortedFeatures.last}")
  }
}
