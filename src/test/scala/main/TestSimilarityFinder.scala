package main

import dataset.ArtistDatasetReader
import org.scalatest.FunSuite

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
  test("similarities one artist dataset") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile()
    for(artist <- artistDataset.keys) {
      println(artist)
//      baseline.VectorRepresentationCreator.compareVectorRepresentationForEntities(artist, artistDataset(artist): _*)
    }
  }
}
