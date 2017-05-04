package core.query.specific

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{KnowledgeGraphs, MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}

/**
  * Created by Espen on 15.11.2016.
  */
object DatasetInferrer {

  val PatternForValueMatchDataset = s"(${SimilarPropertyOntology.valueMatchClass}|${SimilarPropertyOntology.valueMatchCount}|${SimilarPropertyOntology.valueMatchValue})".r.unanchored
  val PatternForSimilarOntology = s"(${SimilarPropertyOntology.spo})".r.unanchored
  val PatternResultSimilarGlobals = s"(${ResultsSimilarArtistsGlobals.values.map(_.toString).mkString("\\|")})".r.unanchored

  def getDataset(query: String)(implicit knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraphs.wikidata => {
        query match {
//          case a if PatternForSimilarOntology.findFirstIn(a).isDefined => KnowledgeGraphs.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph)
          case PatternForValueMatchDataset(_) => return MyDatasets.valueMatchWikidata
          case PatternForSimilarOntology(_) => return KnowledgeGraphs.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph)
          case _ => return MyDatasets.dsWikidata
        }
      }
      case KnowledgeGraphs.dbPedia => {
        query match {
          case PatternForValueMatchDataset(_*) => return MyDatasets.valueMatchDBpedia
          case PatternForSimilarOntology(_*) => return KnowledgeGraphs.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph)
          case _ => return MyDatasets.dsDBpedia
        }
      }
    }
  }

}
