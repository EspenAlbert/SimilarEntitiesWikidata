package core.strategies
import core.feature.Feature
import core.globals.{FeatureType, KnowledgeGraph}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF

/**
  * Created by espen on 31.03.17.
  */
case class SearchUndirectedL1Strategy(property : String, values : List[String]) extends Strategy{
  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return findSimilars()
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val prefix = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    return values.filter(_.startsWith(prefix))
    .map(e => (e -> new Feature(property, FeatureType.searchUndirectedL1, 1, weight)))
    .toMap
  }

  override val name: String = SearchUndirectedL1Strategy.name
}

object SearchUndirectedL1Strategy {
  val name = "SearchUndirectedL1Strategy"
}
