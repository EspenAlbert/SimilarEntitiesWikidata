package main

import java.io.PrintWriter

import breeze.numerics.log
import dataset.ArtistDatasetReader
import displayer.Displayer
import dump.DumpObject
import feature.Feature
import globals.{FeatureType, MyConfiguration, SimilarPropertyOntology}
import org.scalatest.FunSuite
import ranker.Ranker
import rdf.GraphRDF
import strategies._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimilarityFinder extends FunSuite{
  test("similarities for obama") {
    SimilarityFinder.findTopKSimilarTo("w:Q76", 10)
  }
  test("Similars to Lionel Messi") {
    SimilarityFinder.findTopKSimilarTo("w:Q615", 10)
  }
  test("similarities for tufte Q436113") {
    SimilarityFinder.findTopKSimilarTo("w:Q436113", 10)
//    val g = new GraphRDF("w:Q513")
//    println(g.getType)
//    SimilarityFinder.findTopKSimilarTo("w:Q513", 10)
  }
  test("An empty int") {
    val listOfFeatures = List(
      new Feature("dontmatter", featureType = FeatureType.alternativeMatch, 0, -0.5910796644040527),
      new Feature("dontmatter", featureType = FeatureType.alternativeMatch, 0, -0.5910796644040527),
      new Feature("dontmatter", featureType = FeatureType.alternativeMatch, 1, 0.6056784013228625),
      new Feature("dontmatter", featureType = FeatureType.alternativeMatch, 1, 0.6056784013228625),
      new Feature("dontmatter", featureType = FeatureType.alternativeMatch, 1, 18.852515756397306),
      new Feature("dontmatter", featureType = FeatureType.alternativeMatch, 1, 18.852515756397306)
    )
    val otherE = new GraphRDF("http://www.wikidata.org/entity/Q44015")
    val obama = new GraphRDF("http://www.wikidata.org/entity/Q76")
    val strategies = StrategyGenerator.generateStrategies(obama)
    val graphsOfSimilars = List(otherE)
    val featureMap = mutable.Map[String, ListBuffer[Feature]]()
    strategies.map{(s) => SimilarityFinder.addFeaturesToMap(featureMap, s.execute(graphsOfSimilars))}
    val ranked = Ranker.getSortedOrder(featureMap.toMap)
    println(listOfFeatures.sorted)
  }
  test("statistics about artist dataset") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile()
    var statmentsTotal = 0
    val k = 10
    var accuracy = 0.0//of the ones extracted how many is actually relevant
    var recall = 0.0 //% number that are found...

    for(artist <- artistDataset.keys) {
      val graph = new GraphRDF(artist)
      statmentsTotal += graph.statements.size
      val similars = SimilarityFinder.findTopKSimilarTo(artist, k)
      val similarsExpected = artistDataset(artist)
      val overlaps = similars.foldRight(0){(uri, prevSum) => if(similarsExpected.contains(uri.name)) prevSum + 1 else prevSum}
      println(s"Of the simlars found, only : $overlaps was from expected...")
      accuracy += overlaps.toFloat / k
//      baseline.VectorRepresentationCreator.compareVectorRepresentationForEntities(artist, artistDataset(artist): _*)
    }
//    println("On average is key has about: ", statmentsTotal.toFloat / artistDataset.keys.size, " statements")
    println("On average is key has about: ", statmentsTotal.toFloat / 10, " statements")
    println("Performance  (accuracy %): ", accuracy / 10)
  }
  test("Check the scores gathered from the top 10") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile()
    for(artist <- artistDataset.keys.take(5)) {
      val graph = new GraphRDF(artist)
      val similars = SimilarityFinder.findTopKSimilarTo(artist, 10)
      val similarsExpected = artistDataset(artist)
      val strategies = StrategyGenerator.generateStrategies(graph)
      println("checking scores of expected similars")
      val graphsOfSimilars = similarsExpected.map{new GraphRDF(_)}
      val featureMap = mutable.Map[String, ListBuffer[Feature]]()
      strategies.map{(s) => SimilarityFinder.addFeaturesToMap(featureMap, s.execute(graphsOfSimilars))}
      val ranked = Ranker.getSortedOrder(featureMap.toMap)
      Displayer.displayResult(ranked, graphsOfSimilars.length, artist)
      val overlaps = similars.take(10).foldRight(0){(uri, prevSum) => if(similarsExpected.contains(uri.name)) prevSum + 1 else prevSum}
      println(s"Of the simlars found, only : $overlaps was from expected...")
      //      baseline.VectorRepresentationCreator.compareVectorRepresentationForEntities(artist, artistDataset(artist): _*)
    }
  }

  def adjustConfig(parameter: String, configValue: Any) = {
    parameter match {
      case "DateComparison" => MyConfiguration.dateComparisonWeight = configValue.asInstanceOf[Double]
      case "ValueMatch" => MyConfiguration.valueMatchBoost = configValue.asInstanceOf[Double]
      case "DirectLink" => MyConfiguration.directLinkBoost = configValue.asInstanceOf[Double]
      case "AlternativeLink" => MyConfiguration.alternativeLinkNegative = configValue.asInstanceOf[Double]
      case "InANotInB" => MyConfiguration.inANotInBBoost = configValue.asInstanceOf[Double]
      case "InBNotInAGlobal" => MyConfiguration.globalInBNotInABoost = configValue.asInstanceOf[Double]
      case "DoScaling" => MyConfiguration.doScaling = configValue.asInstanceOf[Boolean]
      case "PropertyDivideBy" => MyConfiguration.maximumWeightPropertyMatch = log(SimilarPropertyOntology.maxCountForProperties / configValue.asInstanceOf[Int])
    }
  }

  test("Check precision and recall at different levels") {
    //Params to tune:
    /*
      var inANotInBBoost = -0.3
  var inBNotInABoost = -0.1
     val dateComparisonWeight = 0.3
  val valueMatchBoost = 2
  val directLinkBoost = 5
  val alternativeLinkNegative = -0.1
    var doScaling = true
  val maximumWeightPropertyMatch = log(SimilarPropertyOntology.maxCountForProperties.toString.toInt / 100)
  topResult
  doScaling = false
  alternativeLink = -0.1
  dL= 5.0
  vM = 8.0
  dateComparison = 1.0
  inBNotInA= -0.1
  inANotInB = 0
     */
    val parameterTuning = List(//List(-1, -.5, -.2, 0.0, -.3), List(-2.0, -1.0, -.5, -.25, 0.0, -.1),
//      List(0.1, .5, .8, 1.0, .3),
     // List(5.0, 8.0, 2.0),
 List(3.0, 7.0, 10.0, 5.0), List(5,10,30,50,75,100,150,200))//List(true, false))
    val tuningParameter = List(//"InANotInB", "InBNotInAGlobal",
//     "DateComparison",
     //"ValueMatch",
 "DirectLink", "PropertyDivideBy")//, "DoScaling")
    val artistDataset = ArtistDatasetReader.getDatasetFromFile().take(100)
    for((parameter, i) <- tuningParameter.zipWithIndex) {
      for(config <- parameterTuning(i)) {
        adjustConfig(parameter, config)
        val filename = tuningParameter(i) + config.toString
        doPerformanceTest(artistDataset, filename)
      }
    }
  }
  test("Test a better setup") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile().take(100)
    doPerformanceTest(artistDataset, "setup7pmTuesday1500retrieved")
  }
  test("Do full test AND store result") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile()
    val saveEvery = 100
    val queryEntityStatistics = mutable.HashMap[String, mutable.HashMap[String, Int]]()
    val queryEntityResults = mutable.HashMap[String, mutable.HashMap[String, Double]]()
    val queryEntityResultForExpected = mutable.HashMap[String, List[(String, (Double, Int))]]()
    val levels = Array(1, 10, 20, 30, 40, 50, 100, 150, 200, 250, 500, 1000)
    var fileNumber = 18
    for((artist, i) <- artistDataset.keys.toList.zipWithIndex) {
      if (i < 1700) Unit
      else {
        if ((i + 1) % saveEvery == 0) {
          //TODO: Remember to change back...
          DumpObject.dumpMapStringMapStringInt(queryEntityStatistics, s"qEntityStatistics$fileNumber")
          DumpObject.dumpMapStringMapStringDouble(queryEntityResults, s"qEntityResultSimilarityFinder$fileNumber")
          DumpObject.dumpMapStringListStringDoubleInt(queryEntityResultForExpected, s"qEntityResultExpectedSimilarityFinder$fileNumber")
          println(s"Dumped file # $fileNumber")
          fileNumber += 1
        }
        val (entityGraph: GraphRDF, strategies: Array[Strategy]) = SimilarityFinder.findGraphAndStrategiesForEntity(artist)
        val entityStatistics = getStrategyCounts(strategies)
        entityStatistics += "statementCount" -> entityGraph.statements.size
        queryEntityStatistics += artist -> entityStatistics
        val expectedSimilars = artistDataset(artist)
        val (resultsMap, notFound) = getMapOfRecallPrecisionAndScoreForLevelsPlusNotFound(entityGraph, strategies, levels, expectedSimilars)
        queryEntityResults += artist -> resultsMap
        val graphsOfSimilars = expectedSimilars.map {
          new GraphRDF(_)
        }
        val featureMap = mutable.Map[String, ListBuffer[Feature]]()
        strategies.map { (s) => SimilarityFinder.addFeaturesToMap(featureMap, s.execute(graphsOfSimilars)) }
        val ranked = Ranker.getSortedOrder(featureMap.toMap)
        val expectedResultFacts: List[(String, (Double, Int))] = ranked.map((s) => s.name -> (s.score, graphsOfSimilars.filter(_.entity == s.name)(0).statements.size))
        queryEntityResultForExpected += artist -> expectedResultFacts
        println(s"Finished artist $i of ${artistDataset.keys.size}")
      }
    }
    DumpObject.dumpMapStringMapStringInt(queryEntityStatistics, s"qEntityStatistics$fileNumber")
    DumpObject.dumpMapStringMapStringDouble(queryEntityResults, s"qEntityResultSimilarityFinder$fileNumber")
    DumpObject.dumpMapStringListStringDoubleInt(queryEntityResultForExpected, s"qEntityResultExpectedSimilarityFinder$fileNumber")

  }

  private def getStrategyCounts(strategies: Array[Strategy]) = {
    val statisticsMap = mutable.HashMap[String, Int]()
    statisticsMap += "valueMatch" -> strategies.filter(_.isInstanceOf[ValueMatchStrategy]).length
    statisticsMap += "directLink" -> strategies.filter(_.isInstanceOf[DirectLinkStrategy]).length
    statisticsMap += "propertyMatch" -> strategies.filter(_.isInstanceOf[PropMatchStrategy]).length
    statisticsMap += "dateComparison" -> strategies.filter(_.isInstanceOf[DateComparisonStrategy]).length
    statisticsMap
  }

  private def doPerformanceTest(artistDataset: Map[String, List[String]], filename : String) = {
    val levels = Array(10, 20, 30, 40, 50, 100, 150, 200, 250, 500, 1000)
    val precisions = ArrayBuffer[Double]()
    val recall = ArrayBuffer[Double]()
    for (a <- 0 until levels.length) {
      recall.append(0)
      precisions.append(0)
    }
    val notFoundEntities = mutable.HashMap[String, List[String]]()
    val numberOfTests = artistDataset.size
    for (artist <- artistDataset.keys) {
      val notFound: (String, List[String]) = doTestForArtist(artistDataset, levels, precisions, recall, artist)
      notFoundEntities += notFound
      println("unable to find : ", notFound)
    }
    val averagePrecisions = precisions.map(_ / numberOfTests)
    val averageRecalls = recall.map(_ / numberOfTests)
    val writer = new PrintWriter("output/results2/" + filename + "precisionAndRecall.txt")
    writer.write(notFoundEntities.toString() + "\n")
    for ((level, i) <- levels.zipWithIndex) {
      writer.write(s"$level P:${averagePrecisions(i)} R:${averageRecalls(i)} \n")
    }
    writer.close()
    println(s"Finished setup for $filename")
  }

  private def doTestForArtist(artistDataset: Map[String, List[String]], levels: Array[Int], precisions: ArrayBuffer[Double], recall: ArrayBuffer[Double], artist: String): (String, List[String]) = {
    val similars = SimilarityFinder.findTopKSimilarTo(artist, 10)
    val similarsExpected = artistDataset(artist)
    for ((level, i) <- levels.zipWithIndex) {
      val overlaps = similars.take(level).foldRight(0) { (uri, prevSum) => if (similarsExpected.contains(uri.name)) prevSum + 1 else prevSum }
      precisions(i) += overlaps.toDouble / level
      val recallValue = overlaps.toDouble / similarsExpected.length
      recall(i) += recallValue
      if (i == levels.length - 1) println(s"Recall for artist 1000 received: $artist", recallValue)
    }
    val notFound = artist -> similarsExpected.filterNot((s) => similars.exists(_.name == s))
    notFound
  }
  private def getMapOfRecallPrecisionAndScoreForLevelsPlusNotFound(artist : GraphRDF, strategies : Array[Strategy], levels: Array[Int], similarsExpected : List[String]): (mutable.HashMap[String, Double], List[String]) = {
    val similars = SimilarityFinder.findSimilarToEntityWithStrategies(10, artist, strategies)
    val statisticsMap = mutable.HashMap[String, Double]()
    try {
      for ((level, i) <- levels.zipWithIndex) {
        val overlaps = similars.take(level).foldRight(0) { (uri, prevSum) => if (similarsExpected.contains(uri.name)) prevSum + 1 else prevSum }
        statisticsMap += s"precision@$level" -> overlaps.toDouble / level
        statisticsMap += s"recall@$level" -> overlaps.toDouble / similarsExpected.length
        statisticsMap += s"score@$level" -> similars(level - 1).score
      }
    } catch {
      case a :IndexOutOfBoundsException => println(s"similars to ${artist.entity} was unable to get full 1000 similars")
      case a => println("Unknown exceptin caught ", a.getMessage)
    }

    val notFound = similarsExpected.filterNot((s) => similars.exists(_.name == s))
    return (statisticsMap, notFound)
  }
}
