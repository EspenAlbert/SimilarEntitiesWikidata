package strategies
import feature.Feature
import globals.FeatureType
import query.specific.QueryFactoryV2
import query.variables.OptionsForResultQueryVariable
import rdf.{GraphRDF, SimpleRDFFactory}

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
case class DirectLinkStrategy(property: String, others : Set[String], override val weight: Double) extends Strategy{
//  def fullMatch(tuple: (String, String, String), otherEntity : String): Boolean = {
//    return tuple match {
//      case (`otherEntity`, `property`, `entity`) => true
//      case (`entity`, `property`, `otherEntity`) => true
//      case _ => false
//    }
//  }
  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      if(others.contains(other.entity)) featureMap += entity -> new Feature(property, FeatureType.directLinkMatch, 1, weight)
    }
    return featureMap.toMap
  }

  override def findSimilars(): List[String] = {
    others.toList
//    val statment = if(isSubject) SimpleRDFFactory.getStatement((entity, property, "?o " + OptionsForResultQueryVariable.sameTypeFilter + "_" + rdfType))
//    else SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.sameTypeFilter + "_" + rdfType, property, entity))
//    return QueryFactoryV2.findList(statment)
  }

}
