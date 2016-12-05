package main

import java.io.PrintWriter

import dataset.ArtistDatasetReader
import displayer.Displayer
import feature.Feature
import globals.{FeatureType, MyConfiguration}
import org.scalatest.FunSuite
import ranker.Ranker
import rdf.GraphRDF
import strategies.{Strategy, StrategyGenerator}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimilarityFinder extends FunSuite{
  test("similarities for obama") {
    SimilarityFinder.findTopKSimilarTo("w:Q76", 10)
  }
  test("similarities for tufte Q436113") {
    SimilarityFinder.findTopKSimilarTo("w:Q436113", 10)
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
      case "InBNotInA" => MyConfiguration.inBNotInABoost = configValue.asInstanceOf[Double]
      case "DoScaling" => MyConfiguration.doScaling = configValue.asInstanceOf[Boolean]
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
     */
    val parameterTuning = List(List(-2, -1, -.5, -.25, -.1, 0), List(-2, -1, -.5, -.25, -.1, 0), List(0.1, .3, .5, .8, 1.0),
      List(0, 1, 2, 3, 5, 8), List(0, 1, 3, 5, 7, 10), List(-2, -1, -.5, -.25, -.1, 0), List(true, false))
    val tuningParameter = List("InANotInB", "InBNotInA", "DateComparison", "ValueMatch", "DirectLink", "AlternativeLink", "DoScaling")
    val artistDataset = ArtistDatasetReader.getDatasetFromFile().take(10)
    for((parameter, i) <- tuningParameter.zipWithIndex) {
      for(config <- parameterTuning(i)) {
        adjustConfig(parameter, config)
        val filename = tuningParameter(i) + config.toString
        doPerformanceTest(artistDataset, filename)
      }
    }
  }

  private def doPerformanceTest(artistDataset: Map[String, List[String]], filename : String) = {
    val levels = Array(10, 20, 30, 40, 50, 100, 150, 200, 250, 500, 1000)
    val precisions = ArrayBuffer[Double]()
    val recall = ArrayBuffer[Double]()
    for (a <- 0 until levels.length) {
      recall.append(0)
      precisions.append(0)
    }
    val numberOfTests = 10
    for (artist <- artistDataset.keys.take(numberOfTests)) {
      val similars = SimilarityFinder.findTopKSimilarTo(artist, 10)
      val similarsExpected = artistDataset(artist)
      for ((level, i) <- levels.zipWithIndex) {
        val overlaps = similars.take(level).foldRight(0) { (uri, prevSum) => if (similarsExpected.contains(uri.name)) prevSum + 1 else prevSum }
        precisions(i) += overlaps.toDouble / level
        recall(i) += overlaps.toDouble / similarsExpected.length
      }
    }
    val averagePrecisions = precisions.map(_ / numberOfTests)
    val averageRecalls = recall.map(_ / numberOfTests)
    val writer = new PrintWriter("output/results/" + filename + "precisionAndRecall.txt")
    for ((level, i) <- levels.zipWithIndex) {
      writer.write(s"$level P:${averagePrecisions(i)} R:${averageRecalls(i)} \n")
    }
    writer.close()
  }
}
