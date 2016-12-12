package graphProducer

import baseline.VectorRepresentationCreator
import breeze.linalg.{max, min, sum}
import breeze.numerics.round
import dataset.ArtistDatasetReader
import dump.DumpObject

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by espen on 10-Dec-16.
  */
object GraphDataProducer {

  def findMax(listOfScores: List[(String, (Double, Int))]): Double = {
    return listOfScores.foldRight(0.0){(s, previousScore) => if(s._2._1 > previousScore) s._2._1 else previousScore}
  }

  def findMin(listOfScores: List[(String, (Double, Int))]): Double = {
    return listOfScores.foldRight(999999.0){(s, previousScore) => if(s._2._1 < previousScore) s._2._1 else previousScore}
  }
  def findStatementsWhereMoreThan(statementCount : Int, listOfScores: List[(String, (Double, Int))]) : List[(String, Double)] ={
    listOfScores.filter(_._2._2 > statementCount).map((s) =>(s._1, s._2._1))
  }

  def filterAwayLessThanLevel(score: Double, expectedWithScores: List[(String, Double)]) :List[(String, Double)]= {
//    println("Before : ", expectedWithScores.length)
    val filtered = expectedWithScores.filter(_._2 >= score)
//    println("After : ", filtered.length)
    return filtered
  }

  def countRecalledAtLevel(qEntity: String, qEntityResult: List[(String, (Double, Int))], index: Int, recalledMap: Map[String, mutable.Map[String, ListBuffer[Boolean]]]): Int = {
    return (for(
      r<- qEntityResult;
      recalled = if(recalledMap(qEntity)(r._1)(index)) 1 else 0;
      if(recalled == 1)
    ) yield recalled).length
  }

  def printTrueRecallLevels() = {

    val recalledMap =  getMapOfBooleanRecalled()
    val statementCounts = List(5,10,15,20,30,40,50,75,100,200)
    val recallLevels = List(10, 20, 30, 40, 50, 100)

    val recallScores = (for(
      level <- statementCounts;
      (qEntities, qResultList) = getEntitiesWithMoreThan(level, "recall@100");
      resultsBeforeRecall = qResultList.values.flatten.size;
      index<- 0 until recallLevels.length;
      resultsAfterRecall = qResultList.map{(s) => countRecalledAtLevel(s._1, s._2, index, recalledMap)};
      (score, count) = (sum(resultsAfterRecall) / resultsBeforeRecall.toDouble, resultsBeforeRecall)
    )yield (score, count))
    println(recallScores)
    for((count, i) <- statementCounts.zipWithIndex) {
      val start = i * recallLevels.length
      val end = (i+1) * recallLevels.length
      println(s"$count", recallScores.slice(start, end).map(_._1))
      println(s"entities with more than $count statements", recallScores.slice(start, end).map(_._2))
    }
  }

  private def getMapOfBooleanRecalled() : Map[String, mutable.Map[String, ListBuffer[Boolean]]] = {
    val qEntityResult2 = DumpObject.getQEntityResult()
    val qEntityStatistics = DumpObject.getQEntityStatistics()
    val qEntitiesWithoutRecall = qEntityResult2.filterNot(_._2.contains("recall@100"))//Some have failed to find this
    val qEntityResult = qEntityResult2.filterNot((s) => qEntitiesWithoutRecall.contains(s._1))
    val queryEntityResultForExpected = DumpObject.getQEntityResultForExpected().filterNot((s) => qEntitiesWithoutRecall.contains(s._1))
    val recallLevels = List(10, 20, 30, 40, 50, 100)
    val recallLevelStrings = recallLevels.map((s) => s"recall@$s")
    val megaMap = mutable.Map[String, mutable.Map[String, ListBuffer[Boolean]]]()
    for ((qEntity, resultList) <- queryEntityResultForExpected) {
      val qEntityMap = mutable.Map[String, ListBuffer[Boolean]]()
      for ((resultEntityList, i) <- resultList.zipWithIndex) {
        qEntityMap += resultEntityList._1 -> ListBuffer[Boolean]()
      }
      val similarList = resultList.map(_._1)
      for (rLevel <- recallLevelStrings) {
        val indexIncluded = round(qEntityResult(qEntity)(rLevel) * resultList.length) - 1
        for (i <- 0 until resultList.length) {
          val recalled = if (i <= indexIncluded) true else false
          qEntityMap(similarList(i)).append(recalled)
        }
      }
      megaMap += qEntity -> qEntityMap
    }
    return megaMap.toMap
  }

  def getSetOfEntitiesWithMoreThanXStatements(statementCount : Int): Set[String] = {
    val (qEntityResult, queryEntityResultForExpected, qEntityStatistics) = getWorkingSet
    val qEntitiesWithMoreThanX = qEntityStatistics.filter(_._2("statementCount") > statementCount).map(_._1).toSet
    val moreThanX = queryEntityResultForExpected.filter((s) => qEntitiesWithMoreThanX.contains(s._1)).map(_._2).flatten.filter(_._2._2 > statementCount).map(_._1)
    return moreThanX.toSet ++ qEntitiesWithMoreThanX.toSet
  }

  private def getWorkingSet() : (mutable.HashMap[String, mutable.HashMap[String, Double]],
    mutable.Map[String, List[(String, (Double, Int))]],
    mutable.HashMap[String, mutable.HashMap[String, Int]])= {
    val qEntityResult2 = DumpObject.getQEntityResult()
    val qEntitiesWithoutRecall = qEntityResult2.filterNot(_._2.contains("recall@100"))
    //Some have failed to find this
    val qEntityResult = qEntityResult2.filterNot((s) => qEntitiesWithoutRecall.contains(s._1))
    val queryEntityResultForExpected = DumpObject.getQEntityResultForExpected().filterNot((s) => qEntitiesWithoutRecall.contains(s._1))
    val qEntityStatistics = DumpObject.getQEntityStatistics().filterNot((s) => qEntitiesWithoutRecall.contains(s._1))
//    println(s"${qEntityResult.size}, ${queryEntityResultForExpected.size}, ${qEntityStatistics.size}")
    (qEntityResult, queryEntityResultForExpected, qEntityStatistics)
  }

  def createRecallLevelsMoreThanXStatementsCheat(statementCount : Int, atLevel : String): (Double, Int) = {
    val (qEntityResult: mutable.HashMap[String, mutable.HashMap[String, Double]],
    resultListMoreThanXStatements: mutable.Map[String, List[(String, (Double, Int))]]) =
      getEntitiesWithMoreThan(statementCount, atLevel)
    val recallScores = qEntityResult.map((s)=> s._1 -> s._2(atLevel))
    val afterRecall = (for(
      (qEntity, listOfR)<- resultListMoreThanXStatements;
      filtered = filterAwayLessThanLevel(recallScores(qEntity), listOfR.map((s) => (s._1, s._2._1)))
    ) yield filtered).flatten
    val sizeBefore = resultListMoreThanXStatements.map(_._2).flatten.size
//    println(sizeBefore)
    val sizeAfter = afterRecall.size
    return (sizeAfter / sizeBefore.toDouble, sizeBefore)
//    println(sizeAfter)
//    println(s"Recall level = ${sizeAfter / sizeBefore.toDouble}")
  }

  private def getEntitiesWithMoreThan(statementCount: Int, atLevel: String): (mutable.HashMap[String, mutable.HashMap[String, Double]], mutable.Map[String, List[(String, (Double, Int))]]) = {
    val (qEntityResult2, queryEntityResultForExpected2, qEntityStatistics2) = getWorkingSet
    val notFound = qEntityResult2.filterNot((s) => s._2.contains(atLevel))
    val qEntityResult = qEntityResult2.filterNot((s) => notFound.contains(s._1))
    val queryEntityResultForExpected = queryEntityResultForExpected2.filterNot((s) => notFound.contains(s._1))
    val qEntityStatistics = qEntityStatistics2.filterNot((s) => notFound.contains(s._1))
    val setOfEntitiesWithMoreThanXStatements = getSetOfEntitiesWithMoreThanXStatements(statementCount)
    val qEntityExpectedQEntityHasMoreThanX = queryEntityResultForExpected.filter((s) => setOfEntitiesWithMoreThanXStatements.contains(s._1))
    val resultListMoreThanXStatements = for (
      (qEntity, listOfResult) <- qEntityExpectedQEntityHasMoreThanX;
      filtered = listOfResult.filter((s) => setOfEntitiesWithMoreThanXStatements.contains(s._1))
    ) yield (qEntity, filtered)
    (qEntityResult, resultListMoreThanXStatements)
  }

  def printRecallLevelForStatementCounts() = {
    val statementCounts = List(5,10,15,20,30,40,50,75,100,200)
    val recallLevels = List(10, 20, 30, 40, 50, 100)
    val recallLevelStrings = recallLevels.map((s) => s"score@$s")
    val recallScores = (for(
      level <- statementCounts;
      recallLevel <- recallLevelStrings;
      (score, count) = createRecallLevelsMoreThanXStatementsCheat(level, recallLevel)
    )yield (score, count))
    println(recallScores)
    for((count, i) <- statementCounts.zipWithIndex) {
      val start = i * recallLevels.length
      val end = (i+1) * recallLevels.length
      println(s"$count", recallScores.slice(start, end).map(_._1))
      println(s"entities with more than $count statements", recallScores.slice(start, end).map(_._2))
    }
  }
  def getStatementDistribution() = {
    val queryEntityResultForExpected = DumpObject.getQEntityResultForExpected()
    val qEntityResult = DumpObject.getQEntityResult()
    val qEntityStatistics = DumpObject.getQEntityStatistics()
    val statementCounts = queryEntityResultForExpected.map(_._2.map(_._2._2)).flatten ++ qEntityStatistics.map(_._2("statementCount"))
    println(statementCounts)



  }

  def createXY0to1SimilarityFinder() = {
    val queryEntityResultForExpected = DumpObject.getQEntityResultForExpected()
    val qEntityResult = DumpObject.getQEntityResult()
    def getScalingScore(entity : String): Double = {
      return qEntityResult(entity).filter(_._1 == "score@1").toList(0)._2
    }
    val totalScore = queryEntityResultForExpected.foldRight(0.0){(entry, preSum) => preSum + getAvgScoreSum(entry._2, getScalingScore(entry._1))}
    val maxScores = queryEntityResultForExpected.map((s) => findMax(s._2) / getScalingScore(s._1))
    val minScores = queryEntityResultForExpected.map((s) => findMin(s._2) / getScalingScore(s._1))
    println(s"Total score: $totalScore average score: ${totalScore / queryEntityResultForExpected.size}")
    println(s"Maximum, min scores: ${maxScores.zip(minScores)} ")
    println(s"Average minimum ${minScores.reduce(_ + _) / minScores.size} or is it: ${minScores.foldRight(0.0){(s, b) => s + b} / minScores.size} ")
    println(s"Average max ${maxScores.reduce(_ + _) / maxScores.size}")
    println("maximum max", max(maxScores))
  }
  def print0To1SimilarityScores() = {
    val queryEntityResultForExpected = DumpObject.getQEntityResultForExpected()
    val qEntityResult = DumpObject.getQEntityResult()
    def getScalingScore(entity : String): Double = {
      return qEntityResult(entity).filter(_._1 == "score@1").toList(0)._2
    }
    val scores = (for(
      (qEntity, scoreList) <- queryEntityResultForExpected;
      normalizedScores = scoreList.map(_._2._1 / getScalingScore(qEntity))
    )yield normalizedScores).flatten
    println(scores)
  }
  def createXY0to1Baseline() = {
    val qEntityExpected = ArtistDatasetReader.getDatasetFromFile()
    val listOfScores = qEntityExpected.map((a) => a._1 -> VectorRepresentationCreator.compareVectorRepresentationForEntities(a._1, a._2: _*))
    DumpObject.dumpMapStringListDouble(listOfScores, "baselineScores")
  }
  def getBaselineScores() = {
    val listOfScores = DumpObject.readMapStringListDouble("baselineScores")
    val totalScore = listOfScores.map((s) => s._2.foldRight(0.0){_ + _}).foldRight(0.0){_ + _}
    val numberOfScores = listOfScores.values.foldRight(0)(_.length + _)
    val averageScore = totalScore / numberOfScores
    println(s"Total score $totalScore avg score: $averageScore")
    val minScores = listOfScores.values.map(min(_))
    val maxScores = listOfScores.values.map(max(_))
    println(s"Average minimum ${minScores.reduce(_ + _) / minScores.size} or is it: ${minScores.foldRight(0.0){(s, b) => s + b} / minScores.size} ")
    println(s"Average max ${maxScores.reduce(_ + _) / maxScores.size}")
    println(listOfScores)
    println(max(maxScores))
  }
  def getAllScoresBaseline() = {
    val listOfScores = DumpObject.readMapStringListDouble("baselineScores")
    val filteredScores = listOfScores.map(_._2).flatten
    println(filteredScores.size)
    println(filteredScores)
  }

  private def getAvgScoreSum(results: List[(String, (Double, Int))], scalingScore : Double) = {

    results.foldRight(0.0) { (s, preSum2) => (s._2._1 / scalingScore) + preSum2 } / results.length
  }

}
