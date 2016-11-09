package baseline

import dataset.ArtistDatasetReader
import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestBaselineOnArtistDataset extends FunSuite{
  test("Run tf-idf through similar artists dataset") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile()
    for(artist <- artistDataset.keys) {
      baseline.VectorRepresentationCreator.compareVectorRepresentationForEntities(artist, artistDataset(artist): _*)
    }

  }


}
