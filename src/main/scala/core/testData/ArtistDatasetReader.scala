package core.testData

import core.dump.DumpObject
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{KnowledgeGraphs, SimilarPropertyOntology}
import core.query.specific.{QueryFactory, UpdateQueryFactory}
import core.rdf.GraphRDF

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Created by Espen on 08.11.2016.
  */
object ArtistDatasetReader {

  final val filename = "tenMostSimilarArtists"
  val sampleNameWikidata = "tenMostSimilarArtists-sample-" + KnowledgeGraphs.wikidata
  val sampleNameDBpedia = "tenMostSimilarArtists-sample-" + KnowledgeGraphs.dbPedia
  private def findWikidataId(objectValue: String, musicbrainzIdPropertyName : String): Option[String]= {
    if(failedIds.exists( _ == objectValue)) return None
    try {
      Some(QueryFactory.singleSubjectWithPropertyAndValue(musicbrainzIdPropertyName, objectValue)(KnowledgeGraphs.wikidata))
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
  def uploadDatasetWikidata()(implicit knowledgeGraph: KnowledgeGraph = KnowledgeGraphs.wikidata) : Unit = {
    getDatasetFromFile().foreach{
      case (qE, similars) => UpdateQueryFactory.addExpectedSimilarsForQEntity(qE, similars)
    }
  }
  def getDatasetSampleWikidata(): Map[String, List[String]] = {
    return DumpObject.getStringMap(sampleNameWikidata)
  }
  def getDatasetSampleDBpedia(): Map[String, List[String]] = {
    return DumpObject.getStringMap(sampleNameDBpedia)
  }
  def convertDatasetToDBpediaIds(wikidataMap : Map[String, List[String]]) : Map[String, List[String]] = {
    val prefixDBpedia = KnowledgeGraphs.getDatasetEntityPrefix(KnowledgeGraphs.dbPedia)
    val dbpediaMap = for {
      (artist, similars) <- wikidataMap
      artistId = decodeWikidataId(artist)
      similarsIDs = similars.map(decodeWikidataId(_))
    }yield (artistId, similarsIDs)
    return dbpediaMap.toMap
  }

  private def decodeWikidataId(artist: String): String = {
    //NOT FOUND in DBpedia
    val NotFoundDBpedia : String = SimilarPropertyOntology.w + "Q4570669"
    return QueryFactory.findIdDBpediaFromWikidataId(artist) match {
      case Success(id) => id
      case Failure(_) => artist match {
        case NotFoundDBpedia => return KnowledgeGraphs.getDatasetEntityPrefix(KnowledgeGraphs.dbPedia) + "Tara_MacLean"
      }
    }
  }

  def getDatasetFromFile()(implicit knowledgeGraph: KnowledgeGraph = KnowledgeGraphs.wikidata) : Map[String, List[String]] = {
    println(s"Reading the artist dataset...for $knowledgeGraph")
    knowledgeGraph match {
      case KnowledgeGraphs.wikidata => wikidataDSFull
      case KnowledgeGraphs.dbPedia => getDatasetDBpediaFromFile()
    }
  }

  private def wikidataDSFull: Map[String, List[String]] = {
    val map = DumpObject.getStringMap(filename)
    val mutableMap = mutable.Map[String, List[String]]()
    val myMap = map.filterKeys(isUri).map { case (key: String, values: List[String]) => (key, values.filter(isUri)) }
    println(map.keys.size, " was reduced to: ", myMap.keys.size, " for the keys")
    val totalValues = myMap.values.foldRight(0) { (a, b) => a.length + b }
    println(map.values.foldRight(0)(((a, b) => a.length + b)), " number of values was reduced to: ", totalValues)
    println("Making on average :", totalValues.toFloat / myMap.keys.size, " similars per entity")
    val uniqueArtists = Set[String]() ++ myMap.keys.toSet ++ myMap.values.flatten.toSet
    println(s"Unique artists: ${uniqueArtists.size}")

    return myMap
  }

  val dbPediaFilename = "DBpedia-tenMostSimilarArtists"
  def getDatasetDBpediaFromFile() : Map[String, List[String]] = {
    val map = DumpObject.getStringMap(dbPediaFilename)
    return map
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
  val filenameTypes = "similar-artists-types"
  def findAllPossibleTypesDataset(implicit knowledgeGraph : KnowledgeGraph) : Set[String]= {
    val filename = KnowledgeGraphs.getString(knowledgeGraph) + "-" + filenameTypes
    Try(
      DumpObject.getListString(filename).toSet
    ) match {
      case Success(types) => types
      case Failure(_) => {
        val ds = if(knowledgeGraph == KnowledgeGraphs.wikidata) getDatasetFromFile() else getDatasetDBpediaFromFile()
        val distinctTypes = {
          for {
            (artist, _) <- ds
            graph = new GraphRDF(artist)
            types = graph.getTypes
          } yield types
        }.flatten.toSet
        DumpObject.dumpListString(distinctTypes.toList, filename)
        distinctTypes
      }
    }
  }
  def filenameChallengingTypes(knowledgeGraph: KnowledgeGraph) : String = {
    s"artists-challenge-type-$knowledgeGraph"
  }
  def getChallengingTypesDatasetFromFile()(implicit knowledgeGraph: KnowledgeGraph) : Map[String, List[String]] = {
    DumpObject.getStringMap(filenameChallengingTypes(knowledgeGraph))
  }
}
