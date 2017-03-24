package core.strategies
import core.feature.{Feature, ValueMatchFeature}
import core.feature.ValueMatchFeature
import core.globals.FeatureType
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.QueryFactory
import core.rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
case class ValueMatchStrategy(property: String, isSubject: Boolean, value : String, rdfTypes : List[String], dbCount: Int) extends Strategy{

  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      if(isSubject) {
        for(a <- other.statements) {
          a match {
            case (`entity`, `property`, `value`) => featureMap += entity -> new ValueMatchFeature(property, FeatureType.valueMatch, 1, weight, value)
            case _=> Unit
          }
        }
      } else {
        for(a <- other.statements) {
          a match {
            case (`value`, `property`, `entity`) => featureMap += entity -> new ValueMatchFeature(property, FeatureType.valueMatch, 1, weight, value)
            case _ => Unit
          }
        }
      }
    }
    return featureMap.toMap
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val entities = if(isSubject) QueryFactory.subjectsOfTypeWithPropertyAndValue(property, value, rdfTypes) else
      QueryFactory.objectsOfTypeWithPropertyAndSubject(property, value, rdfTypes)
    return entities.map(e=> e-> new ValueMatchFeature(property, FeatureType.valueMatch, 1, weight, value)).toMap
  }
  override def toString: String = {
    s"ValueMatchStrategy for : $property isSubject=$isSubject value=$value, weight=$weight" + super.toString
  }

}
