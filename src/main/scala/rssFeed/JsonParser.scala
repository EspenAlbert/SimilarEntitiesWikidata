package rssFeed
import core.globals.KnowledgeGraph
import core.interlinking.Interlink
import play.api.libs.json._
import similarityFinder.SimilarityRunner
import similarityFinder.displayer.QueryFactorySimilarityResult

import scala.io.Source
import scala.util.Success
/**
  * Created by espen on 26.04.17.
  */
object JsonParser {
  case class DBpResultJson(startOffset : Int, endOffset: Int, underlyingString: String, types: List[DBpResultTypeJson])
//  case class DBpResultJson(startOffset : Int, endOffset: Int, underlyingString: String, entityType: String, types : List[DBpResultTypeJson])
//  case class DBpResultTypeJson(entityLabel : String, entityURI : String, typeLabel : String, typeURI: String, provenance : String, classificationConfidence : Confidence, linkingConfidence : Confidence, salience : Salience)
  case class DBpResultTypeJson(entityLabel : String, entityURI : String, provenance : String, classificationConfidence : Confidence, linkingConfidence : Confidence, salience : Salience)
  //The level of salience determines whether or not the document is about the entity
  //classLabel =
  // most_salient - A most prominent entity with highest focus of attention in the document.
//  less_salient - A less prominent entity with focus of attention in some parts of the document.
//    not_salient - The document is not about the entity.
  // Score: the entity salience score. High salience score indicates higher focus of attention.
  // Confidence: estimated confidence (probability) that the entity salience class is correct.

  case class Salience(classLabel: String, score : Double, confidence : Double)
  //Linking confidence is the estimated probability of the entityURI being correct given the surface form of the entity.
  //Classification confidence is the estimated probability that the typeLabel is correct for given entityURI.
  case class Confidence(value : Double)


  def findMainEntityAndOtherEntities(filename: String): Seq[(String, String)] = {
    val rawJson = Source.fromFile("input/news/bbcJson/" + filename, "utf-8").getLines().toList.head
    val parsed = Json.parse(rawJson)
    implicit val salienceReader = Json.reads[Salience]
    implicit val confidenceReader = Json.reads[Confidence]
    implicit val rTypeReader = Json.reads[DBpResultTypeJson]
    implicit val rReader = Json.reads[DBpResultJson]
    val results = (parsed).as[List[DBpResultJson]]
    val mostConfident = results.map(r => {
      (r.underlyingString, r.types.filter(_.entityURI.startsWith("http://dbpedia")).maxBy(_.linkingConfidence.value))
    })
    println("----- News entities mapped -------")
    mostConfident.foreach(println)
    val (textInOriginal, mainEntityDbpResultType) = decideMostSalient(mostConfident)
    val mainEntityURI = mainEntityDbpResultType.entityURI
    val wikidataURI = Interlink.fromDBpediaToWikidata(mainEntityURI)
    val otherEntities = mostConfident.map{
      case (underlyingString, dbpResultType) => (underlyingString, Interlink.fromDBpediaToWikidata(dbpResultType.entityURI))
    }
    return (textInOriginal, wikidataURI) :: otherEntities

//    println(s"Main entity: \n In dbpedia: $mainEntityURI \nIn wikidata: $wikidataURI")
//    implicit val knowledgeGraph = KnowledgeGraph.wikidata
//    val topTenSimilar = SimilarityRunner.findTopTenMostSimilarToUsingBFS2(wikidataURI)
//    println(s"Top ten most similars: ${topTenSimilar.mkString("\n")}")
//    val labels = topTenSimilar.map(QueryFactorySimilarityResult.findLabelForEntity).collect { case Success(label) => label }
//    println(s"Labels: $labels")
//    val newSummaries = labels.map(l => originalSummarySingleLine.replace(textInOriginal, l))
//    println(s"Adjusted summaries: \n ${newSummaries.mkString("\n")}")
  }

  val MostSalient = "most_salient"


  def decideMostSalient(stringToMappedEntity: List[(String, DBpResultTypeJson)]): (String, DBpResultTypeJson) = {
    val mainEntity = stringToMappedEntity.filter(_._2.salience.classLabel == MostSalient)
    .maxBy(_._2.salience.score)
     return mainEntity
  }


}
