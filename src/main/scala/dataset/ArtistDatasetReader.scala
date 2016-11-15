package dataset

import dump.DumpObject
import query.specific.QueryFactoryV2
import query.variables.DynamicQueryVariable
import rdf.SimpleRDFFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Espen on 08.11.2016.
  */
object ArtistDatasetReader {
  final val filename = "tenMostSimilarArtists"
  private def findWikidataId(objectValue: String, musicbrainzIdPropertyName : String): Option[String]= {
    val dynamicVariable = new DynamicQueryVariable("s", false)
    if(failedIds.exists( _ == objectValue)) return None
    try {
     Some(QueryFactoryV2.findSingleValue(SimpleRDFFactory.getStatement("?s",musicbrainzIdPropertyName, objectValue))) } catch {
      case _ => None
    }
  }
  val failedIds = mutable.HashSet[String]()
  def readDataset() : mutable.Map[String, List[String]] = {
    val lines = Source.fromFile("input/artists10similar.txt").getLines()
    val musicBrainzProperty = "w:P434"
    val tenMostSimilarForArtist = mutable.Map[String, List[String]]()
    for(line <- lines) {
      val (subject, similars) = (line.split("\t")(0), line.split("\t")(1))
      val similarsList = similars.split(" ")
      val tenSimilars = ArrayBuffer[String]()
      val subjectWikidataID: String = getStringFromOption(subject, findWikidataId(subject, musicBrainzProperty))
      for(similar <- similarsList) {
        val option = findWikidataId(similar, musicBrainzProperty)
        tenSimilars.append(getStringFromOption(similar, option))
      }
      tenMostSimilarForArtist(subjectWikidataID) = tenSimilars.toList
    }
    print(failedIds)
    return tenMostSimilarForArtist
  }
  def writeDatasetConvertedToFile() : Unit = {
    DumpObject.dumpJsonMapStringListString(readDataset().toMap, filename)
  }
  def getDatasetFromFile() : Map[String, List[String]] = {
    return DumpObject.getStringMap(filename)
  }


  def getStringFromOption(similar: String, option: Option[String]): String = {
    option match {
      case Some(x) => return x
      case None => println("Failed to find entity: " + similar); failedIds.add(similar); return similar
    }
  }
}
