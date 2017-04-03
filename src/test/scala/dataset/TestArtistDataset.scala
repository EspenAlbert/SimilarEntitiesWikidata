package dataset

import core.feature.Feature
import core.globals.KnowledgeGraph
import iAndO.dataset.ArtistDatasetReader
import iAndO.dump.DumpObject
import org.scalatest.FunSuite
import tags.{ActiveSlowTag, ActiveTag, TestOnlyTag}

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
  test("get dataset from file", ActiveSlowTag) {
    val dataset = ArtistDatasetReader.getDatasetFromFile()
    val ringoStarrId = "http://www.wikidata.org/entity/Q2632"
    val rihanna = "http://www.wikidata.org/entity/Q36844"
    printStats(dataset, ringoStarrId)
    printStats(dataset, rihanna)
//    assert(dataset.keys.toList.length == 2363)
  }
  test("get dataset with dbpedia ids", ActiveSlowTag) {
    val dataset = ArtistDatasetReader.getDatasetFromFile()
    val dbPediaDS = ArtistDatasetReader.convertDatasetToDBpediaIds(dataset)
    DumpObject.dumpJsonMapStringListString(dbPediaDS, KnowledgeGraph.dbPedia + "-tenMostSimilarArtists")
  }

  test("get dataset with dbpedia ids from file", ActiveSlowTag) {
    val dataset = ArtistDatasetReader.getDatasetDBpediaFromFile()
    val datasetWd = ArtistDatasetReader.getDatasetFromFile()
    assert(dataset.size == datasetWd.size)
  }




  private def printStats(dataset: Map[String, List[String]], ringoStarrId: String) = {
    val ringosSimilars = dataset.getOrElse(ringoStarrId, print("fail")).asInstanceOf[List[String]]
    println(s"${Feature.findLabel(ringoStarrId)} -- ${ringoStarrId}\n ----SIMILARS----\n")
    for (s <- ringosSimilars) {
      println(s"${Feature.findLabel(s)} -- ${s}")
    }
  }
}
