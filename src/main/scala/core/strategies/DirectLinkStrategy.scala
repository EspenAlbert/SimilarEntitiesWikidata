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
//  def fullMatch(tuple: (String, String, String), otherEntity : String): Boolean = {
//    return tuple match {
//      case (`otherEntity`, `property`, `entity`) => true
//      case (`entity`, `property`, `otherEntity`) => true
//      case _ => false
//    }
//  }
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
//    val statment = if(isSubject) SimpleRDFFactory.getStatement((entity, property, "?o " + OptionsForResultQueryVariable.sameTypeFilter + "_" + rdfType))
//    else SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.sameTypeFilter + "_" + rdfType, property, entity))
//    return QueryFactoryV2.findList(statment)
  }

  override def toString: String = {
    s"Direct Link for : $property, weight=$weight" + super.toString
  }

}
