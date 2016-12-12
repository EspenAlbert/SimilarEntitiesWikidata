package graphDataProducer

import graphProducer.GraphDataProducer
import org.scalatest.FunSuite

/**
  * Created by espen on 10-Dec-16.
  */
class TestGraphDataProducer  extends FunSuite{
  test("Method runs") {
    GraphDataProducer.createXY0to1SimilarityFinder()
  }
  test("Baseline creates scores") {
    GraphDataProducer.createXY0to1Baseline()
  }
  test("Get Baseline scores") {
    GraphDataProducer.getBaselineScores()
  }
  test("Create recall levels for entities with more than 30 statements") {
    GraphDataProducer.createRecallLevelsMoreThanXStatementsCheat(30, "score@1000")
  }
  test("Print out all baseline scores") {
    GraphDataProducer.getAllScoresBaseline()
  }
  test("Print out all 0-1 similarity scores") {
    GraphDataProducer.print0To1SimilarityScores()
  }
  test("Print recall levels different statement counts") {
    GraphDataProducer.printRecallLevelForStatementCounts()
  }
  test("Print statement distribution") {
    GraphDataProducer.getStatementDistribution()
  }
  test("Print true recall levels") {
    GraphDataProducer.printTrueRecallLevels()
  }
  test("set addition") {
    val a = Set(1,2,3)
    val b = Set(1,2,5,6)
    println(a ++ b)
  }

}
