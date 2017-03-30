package similarityFinder.displayer

/**
  * Created by espen on 30.03.17.
  */
object ResultHandler {
  def calculateRecall: Unit = {
    val runs = QueryFactorySimilarityResult.findAllRuns()
    val v = for{
      r <- runs
      (qEntity, (recalled, notRecalled, _, _)) <- QueryFactorySimilarityResult.findResultsForRun(r)
      found = recalled.size
      total = recalled.size + notRecalled.size
    } yield (r,found, total)
    val runsGrouped = v.groupBy(_._1)
    val recallLevels = for {
      (run, listOfValues) <- runsGrouped
      recalledEntities: Int = listOfValues.foldLeft(0)((acc, nextValue) => acc + nextValue._2)
      totalEntities: Int = listOfValues.foldLeft(0)((acc, nextValue) => acc + nextValue._3)
      recall = recalledEntities.toDouble / totalEntities
    } yield (run, recall)
    println(recallLevels.mkString("\n"))
  }

}
