package core.globals

import core.testData.WikidataFactory

/**
  * Created by espen on 02.05.17.
  */
object KnowledgeGraphs extends Enumeration {
  def getTopLevelType(knowledgeGraph: KnowledgeGraph) = knowledgeGraph match {
    case KnowledgeGraphs.wikidata => WikidataFactory.entityType
    case KnowledgeGraphs.dbPedia => ExternalURIs.owlThing
  }

  def findKnowledgeGraphFromId(id: String): KnowledgeGraph = id match {
    case a if(a.startsWith(KnowledgeGraphs.getDatasetEntityPrefix(KnowledgeGraphs.wikidata))) => KnowledgeGraphs.wikidata
    case b if(b.startsWith(KnowledgeGraphs.getDatasetEntityPrefix(KnowledgeGraphs.wikidata))) => KnowledgeGraphs.dbPedia
    case c => throw new Exception(s"Failed to find knowledge graph from $id")
  }


  def getSubclassProperty(knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraphs.wikidata => return subclassOfPropertyWikidata
      case KnowledgeGraphs.dbPedia => return SimilarPropertyOntology.rdfsSubclassOf
    }
  }

  val dbpResource = "http://dbpedia.org/resource/"
  val typePropertyWikidata = WikidataFactory.w + "P31"
  val subclassOfPropertyWikidata = WikidataFactory.w + "P279"

  type KnowledgeGraph = Value
  val wikidata: KnowledgeGraph = Value("wikidata")
  val dbPedia: KnowledgeGraph = Value("DBpedia")
  def findDatasetForStoringStrategiesAndMetadata(knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraphs.wikidata => return MyDatasets.strategyMappingWikidata
      case KnowledgeGraphs.dbPedia =>  return MyDatasets.strategyMappingDBpedia
    }
  }
  def getTypeProperty(knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraphs.wikidata => typePropertyWikidata
      case KnowledgeGraphs.dbPedia => SimilarPropertyOntology.rdfType
    }
  }
  def getDatasetEntityPrefix(knowledgeGraph: KnowledgeGraph): String = {
    knowledgeGraph match {
      case KnowledgeGraphs.wikidata => WikidataFactory.w
      case KnowledgeGraphs.dbPedia => dbpResource
    }
  }
  def getMapPropToPropTypeFilename(knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraphs.wikidata => "wikidata-propToTypeMapping"
      case KnowledgeGraphs.dbPedia => "DBpedia-propToTypeMapping"
    }
  }
  val qualifierProperty = """P\d+q$""".r
  def getPropertyWithoutQualifierName(property : String) : String = qualifierProperty.findFirstIn(property) match {
    case Some(s) => s"${KnowledgeGraphs.getDatasetEntityPrefix(KnowledgeGraphs.wikidata)}${s.init}"
    case None => property
  }
  implicit def getString(knowledgeGraph: KnowledgeGraph) : String = {
    return knowledgeGraph.toString
  }
}
