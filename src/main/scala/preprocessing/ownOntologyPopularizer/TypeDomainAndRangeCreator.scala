package preprocessing.ownOntologyPopularizer

import core.globals.{KnowledgeGraph, MyDatasets, SimilarPropertyOntology}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.{AskQuery, QueryFactory, UpdateQueryFactory}
import iAndO.factoryMethods.IOFactory

/**
  * Created by espen on 19.04.17.
  */
object TypeDomainAndRangeCreator {
  def findAndStoreDomainAndRangeTypesForProperty(property: String)(implicit knowledgeGraph: KnowledgeGraph): Unit = {
    val domains = QueryFactory.findDomainTypesForProperty(property)
    val ranges = QueryFactory.findRangeTypesForProperty(property)
    UpdateQueryFactory.addDomainAndRangeTypesForProperty(domains, ranges, property)
  }
  def main(args: Array[String]): Unit = {
//    val knowledgeGraphs = List(KnowledgeGraph.wikidata, KnowledgeGraph.dbPedia)
    val knowledgeGraphs = List(KnowledgeGraph.dbPedia)
    for {
      kg <- knowledgeGraphs
      properties = IOFactory.getAllItemProperties(kg)
      p <- properties
      if p != KnowledgeGraph.getTypeProperty(kg)
      if !AskQuery.ask(() => s"?s <${SimilarPropertyOntology.isDomainType}> <$p>", MyDatasets.strategyMappingDBpedia)(kg)
    } {
      findAndStoreDomainAndRangeTypesForProperty(p)(kg)
    }
  }


}
