package dataset

import core.feature.Feature
import iAndO.dataset.ArtistDatasetReader
import org.scalatest.FunSuite
import tags.{ActiveTag, TestOnlyTag}

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
    val ringoStarrId = "http://www.wikidata.org/entity/Q2632"
    val rihanna = "http://www.wikidata.org/entity/Q36844"
    printStats(dataset, ringoStarrId)
    printStats(dataset, rihanna)
//    assert(dataset.keys.toList.length == 2363)
  }
  test("check intersection of two lists") {
    val list1 = List(1, 2, 3)
    val list2 = List(3,4,5)
    val intersect = list1.intersect(list2)
    assert(intersect == List(3))
    //Checking strings
    val list3 = List("1, 2", "3")
    val list4 = List("3","4,5")
    val intersect2 = list3.intersect(list4)
    assert(intersect2 == List("3"))
    val a = Map("a" -> (List(1), "2"))
    val mapped = a.map{case (c, (d, e)) => (c+ e) -> d}
    assert(mapped == Map("a2" -> List(1)))
    val b = Map("b" -> (List(2), "3"))
    for(i <- a.zip(b)) {
      i match {
        case ((c, (d,e)), (f, (g,h))) => assert(h== "3")
      }
    }
    val intersectionOfLists = a.zip(b).map{case ((c, (d,e)), (f, (g,h))) => d.intersect(g).size}
    println(intersectionOfLists)
    assert(intersectionOfLists.head == 0)


  }


  private def printStats(dataset: Map[String, List[String]], ringoStarrId: String) = {
    val ringosSimilars = dataset.getOrElse(ringoStarrId, print("fail")).asInstanceOf[List[String]]
    println(s"${Feature.findLabel(ringoStarrId)} -- ${ringoStarrId}\n ----SIMILARS----\n")
    for (s <- ringosSimilars) {
      println(s"${Feature.findLabel(s)} -- ${s}")
    }
  }
}
