package core.strategies

import core.feature.Feature
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{FeatureType, KnowledgeGraph}
import core.query.specific.QueryFactory
import core.rdf.GraphRDF

/**
  * Created by espen on 31.03.17.
  */
case class SearchDirectedL2Strategy(property : String, values : List[String]) extends Strategy{
  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return findSimilars()
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val prefix = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    val l1Values = filterOnlyEntities(prefix, values)
    val l2Values = l1Values.keys.flatMap(QueryFactory.findPropertiesAndObjects(_)._1)
    return l1Values ++ filterOnlyEntities(prefix, l2Values)
  }

  private def filterOnlyEntities(prefix: String, values : Iterable[String]): Map[String, Feature] = {
    return values.filter(_.startsWith(prefix))
      .map(e => (e -> new Feature(property, FeatureType.searchDirectedL2, 1, weight)))
      .toMap
  }

  override val name: String = SearchDirectedL2Strategy.name
}

object SearchDirectedL2Strategy {
  val name: String = "SearchDirectedL2Strategy"
}



