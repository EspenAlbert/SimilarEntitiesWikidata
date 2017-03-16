package core.globals


/**
  * Created by Espen on 04.11.2016.
  */
object MyDatasets{
  val DBpediaDS = "dsDBpedia"


  val valueNodeDs = "valueNodeDs"
  final val DsBig = "dsBig"

  val similarProperties2 = "similarProperties2"
  final val Wikidata = "ds"
  final val SimilarProperties = "similarProperties"
  final val ValueMatch = "valueMatch"
  final val ResultsSimilarArtists = "resultsSimilarArtists"



}

object KnowledgeGraph extends Enumeration {
  type KnowledgeGraph = Value
  val wikidata: KnowledgeGraph = Value("wikidata")
  val dbPedia: KnowledgeGraph = Value("DBpedia")
}
