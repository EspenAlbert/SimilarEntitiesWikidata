package dataset

import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestArtistDataset extends FunSuite{
  test("Dataset created without errors...") {
    val map = ArtistDatasetReader.readDataset()
    print(map)
    assert(map.keys.toList.length == 2363)
//    #assert(map.values.foreach(_.length == 10))
  }
  test("write datset to file") {
    ArtistDatasetReader.writeDatasetConvertedToFile()
  }
  test("get dataset from file") {
    val dataset = ArtistDatasetReader.getDatasetFromFile()
    assert(dataset.keys.toList.length == 2363)
  }


}
