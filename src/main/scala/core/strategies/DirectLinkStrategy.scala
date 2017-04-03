package core.strategies
import core.feature.Feature
import core.globals.FeatureType
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
case class DirectLinkStrategy(property: String, others : Set[String]) extends Strategy{

  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    if(otherEntities == Nil) {
      return others.map(o => o -> new Feature(property, FeatureType.directLinkMatch, 1, weight)).toMap
    }
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      if(others.contains(other.entity)) featureMap += entity -> new Feature(property, FeatureType.directLinkMatch, 1, weight)
    }
    return featureMap.toMap
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return execute(Nil)
  }

  val name = DirectLinkStrategy.name
  override def toString: String = {
    s"$name for : $property, weight=$weight" + super.toString
  }

}

object DirectLinkStrategy {
  val name = "DirectLinkStrategy"

}
