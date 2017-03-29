package core.query.specific

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{KnowledgeGraph, MyDatasets, SimilarPropertyOntology}

/**
  * Created by Espen on 15.11.2016.
  */
object DatasetInferrer {

  val patternForValueMatchDataset = ("" + SimilarPropertyOntology.valueMatchClass + "|" +SimilarPropertyOntology.valueMatchCount + "|" + SimilarPropertyOntology.valueMatchValue + "").r

  def getDataset(query: String)(implicit knowledgeGraph: KnowledgeGraph) : String = {
    knowledgeGraph match {
      case KnowledgeGraph.wikidata => {
        return patternForValueMatchDataset.findFirstIn(query) match {
          case Some(s) => MyDatasets.ValueMatchWikidata
          case None => MyDatasets.dsWikidata
      }
    }
      case KnowledgeGraph.dbPedia => {
        return patternForValueMatchDataset.findFirstIn(query) match {
          case Some(s) => MyDatasets.valueMatchDBpedia
          case None => MyDatasets.DBpediaDS
          }
        }
      case _ => throw new Exception("Couldn't find which knowledge graph you are using!")
    }
  }

}
