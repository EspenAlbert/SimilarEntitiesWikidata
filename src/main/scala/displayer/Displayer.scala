package displayer

import ranker.SimilarEntity

/**
  * Created by Espen on 09.11.2016.
  */
object Displayer {
  def displayResult(result : List[SimilarEntity], topK : Int, entity : String) : Unit = {
    println(s"Most similar to $entity is: ")
    for((r, i) <- result.slice(0, topK).zipWithIndex) {
      println(s"Most similar #$i is: ${r.name} with similarity score : ${r.score}")
      try {
        printFeatureInfo(r)
      } catch {
        case a : Throwable => Unit
      }
    }
  }

  private def printFeatureInfo(r: SimilarEntity) = {
    println(s"Most important feature was ${r.sortedFeatures(0)}")
    println(s"2nd most important feature was ${r.sortedFeatures(1)}")
    println(s"3rd most important feature was ${r.sortedFeatures(2)}")
    println(s"4th most important feature was ${r.sortedFeatures(3)}")
    println(s"Minimum import feature was ${r.sortedFeatures.last}")
  }
}
