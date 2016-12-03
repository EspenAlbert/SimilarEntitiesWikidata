package main

import dataset.ArtistDatasetReader
import displayer.Displayer
import feature.Feature
import org.scalatest.FunSuite
import ranker.Ranker
import rdf.GraphRDF
import strategies.{Strategy, StrategyGenerator}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
}
