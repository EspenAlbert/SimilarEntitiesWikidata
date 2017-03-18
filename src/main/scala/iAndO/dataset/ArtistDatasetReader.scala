package iAndO.dataset

import core.globals.KnowledgeGraph
import core.query.specific.QueryFactory
import iAndO.dump.DumpObject

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * Created by Espen on 08.11.2016.
  */
object ArtistDatasetReader {
  final val filename = "tenMostSimilarArtists"
  private def findWikidataId(objectValue: String, musicbrainzIdPropertyName : String): Option[String]= {
    if(failedIds.exists( _ == objectValue)) return None
    try {
      Some(QueryFactory.singleSubjectWithPropertyAndValue(musicbrainzIdPropertyName, objectValue)(KnowledgeGraph.wikidata))
    } catch {
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
    println("Reading the artist dataset...")
    val map = DumpObject.getStringMap(filename)
    val mutableMap = mutable.Map[String, List[String]]()
    val myMap = map.filterKeys(isUri).map{case (key : String, values : List[String]) => (key, values.filter(isUri))}
    println(map.keys.size, " was reduced to: ", myMap.keys.size, " for the keys")
    val totalValues = myMap.values.foldRight(0) { (a, b) => a.length + b }
    println(map.values.foldRight(0)(((a, b) => a.length + b)), " number of values was reduced to: ", totalValues)
    println("Making on average :", totalValues.toFloat / myMap.keys.size , " similars per entity")
    val uniqueArtists = Set[String]() ++ myMap.keys.toSet ++ myMap.values.flatten.toSet
    println(s"Unique artists: ${uniqueArtists.size}")
    return myMap
  }

  def isUri(value : String) : Boolean = {
    return value.startsWith("http")
  }

  def getStringFromOption(similar: String, option: Option[String]): String = {
    option match {
      case Some(x) => return x
      case None => println("Failed to find entity: " + similar); failedIds.add(similar); return similar
    }
  }
}
