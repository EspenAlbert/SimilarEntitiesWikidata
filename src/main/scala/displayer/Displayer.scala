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
    }
  }

}
