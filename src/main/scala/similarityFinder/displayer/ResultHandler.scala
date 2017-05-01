package similarityFinder.displayer

import core.query.specific.UpdateQueryFactory

import scala.collection.immutable
import scala.util.{Failure, Success, Try}

/**
  * Created by espen on 30.03.17.
  */
object ResultHandler {

  def statsExist(run: String): Boolean = {
    Try(QueryFactorySimilarityResult.findStatsForRun(run)) match {
      case Success(s) => true
      case Failure(e) => false
    }
  }
  def notInvalid(run: String) : Boolean = {
    Try(QueryFactorySimilarityResult.findResultsForRun(run)) match {
      case Success(_) => true
      case Failure(_) => false
    }
  }

  def calculateRecall(runName : String*): Unit = {
    val runs = if(runName == Seq()) QueryFactorySimilarityResult.findAllRuns() else runName
    val (calculatedRuns, notCalculatedRuns) = runs.partition(statsExist)
    println(s"Already calculated: $calculatedRuns")
    val statsForRuns: immutable.Iterable[(String, Double, Double, Int, Int, Int)] = calculateStatsForRun(notCalculatedRuns)
    val statsColumns = List("RunName", "Recall", "Precision", "Avg. ExecTime", "Avg. #EntitiesFound", "% of queryTimeout")
    println(statsColumns)
    println(statsForRuns.mkString("\n"))
    statsForRuns.foreach(stat => UpdateQueryFactory.addStatsForRun(stat))
    calculatedRuns.foreach(runName => println(runName, QueryFactorySimilarityResult.findStatsForRun(runName)))
  }

  private def calculateStatsForRun(notCalculatedRuns: Seq[String]) = {
    val v = for {
      r <- notCalculatedRuns.filter(notInvalid) //.filter(_.endsWith("SampleRun"))
      (qEntity, (recalled, notRecalled, execTime, foundEntitiesCount, hadTimeout)) <- QueryFactorySimilarityResult.findResultsForRun(r)
      found = recalled.size
      total = recalled.size + notRecalled.size
    } yield (r, found, total, execTime, foundEntitiesCount, hadTimeout)
    val runsGrouped = v.groupBy(_._1)
    val statsForRuns = for {
      (run, listOfValues) <- runsGrouped
      recalledEntities: Int = listOfValues.foldLeft(0)((acc, nextValue) => acc + nextValue._2)
      totalEntities: Int = listOfValues.foldLeft(0)((acc, nextValue) => acc + nextValue._3)
      recall = recalledEntities.toDouble / totalEntities
      totalExecutionTime = listOfValues.foldLeft(0)((acc, nextValue) => acc + nextValue._4)
      queriesCount: Int = listOfValues.size
      averageExecutionTime = totalExecutionTime / queriesCount
      totalFoundEntities = listOfValues.foldLeft(0)((acc, nextValue) => acc + nextValue._5)
      precision = recalledEntities.toDouble / totalFoundEntities
      averageFoundEntities = totalFoundEntities / queriesCount
      timeouts = listOfValues.count(_._6)
      percentTimedOut = BigDecimal((timeouts.toDouble / queriesCount) * 100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toInt
    } yield (run, recall, precision, averageExecutionTime, averageFoundEntities, percentTimedOut)
    statsForRuns
  }
  private def findNotFoundPairsForRun(runName: String) : Unit = {

  }
}
