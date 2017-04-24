package preprocessing.ownOntologyPopularizer

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{KnowledgeGraph, MyDatasets}
import core.query.specific.{DatasetInferrer, QueryFactory, UpdateQueryFactory}
import core.strategies.StrategyFactory

/**
  * Created by espen on 24.04.17.
  */
object ValueMatchCounter {
  def main(args: Array[String]): Unit = {
    val kgs = List(KnowledgeGraph.wikidata, KnowledgeGraph.dbPedia)
    kgs.foreach(countValueMatches(_))
  }

  def countValueMatches(implicit knowledgeGraph : KnowledgeGraph) {
    val dataset = if(knowledgeGraph == KnowledgeGraph.wikidata) MyDatasets.ValueMatchWikidata else MyDatasets.valueMatchDBpedia
    UpdateQueryFactory.cleanDataset(dataset)
    val sFactory = new StrategyFactory()
    val descriptiveProperties = sFactory.mapPropertyToIsDescriptive.collect{case(p,true) => p}
    for{
      p <- descriptiveProperties
      (entity,count) <- QueryFactory.findObjectsOfPropertyWhereCountGreaterThanThreshold(p) }
      {
        UpdateQueryFactory.updateValueCount(p, entity, count)
    }
  }

}
