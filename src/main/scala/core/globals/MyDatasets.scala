package core.globals


/**
  * Created by Espen on 04.11.2016.
  */
object MyDatasets{
  val DBpediaInterlink = "mappingDBpediaWikidata"

  val ValueMatchDBpedia = "valueMatchDBpedia"

  val DBpediaDS = "dsDBpedia"
  val strategyMappingWikidata = "strategyMappingWikidata"

  val valueNodeDs = "valueNodeDs"
  final val DsBig = "dsBig"
  final val dsWikidata = "dsWikidata"

  val similarProperties2 = "similarProperties2"
  final val Wikidata = "ds"
  final val SimilarProperties = "similarProperties"
  final val ValueMatchWikidata = "valueMatchWikidata"
  final val ResultsSimilarArtists = "resultsSimilarArtists"



}

object KnowledgeGraph extends Enumeration {

  type KnowledgeGraph = Value
  val wikidata: KnowledgeGraph = Value("wikidata")
  val dbPedia: KnowledgeGraph = Value("DBpedia")
  def findDatasetForStoringStrategiesAndMetadata(knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraph.wikidata => return MyDatasets.strategyMappingWikidata
      case KnowledgeGraph.dbPedia =>  throw new NotImplementedError()
    }
  }
  def getTypeProperty(knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraph.wikidata => SimilarPropertyOntology.w + "P31"
      case KnowledgeGraph.dbPedia => SimilarPropertyOntology.rdfType
    }
  }
}
