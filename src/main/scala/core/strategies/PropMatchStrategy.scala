package core.strategies

import core.feature.Feature
import core.globals.FeatureType
import core.query.specific.QueryFactory
import core.rdf.GraphRDF

import scala.collection.mutable
/**
  * Created by Espen on 10.11.2016.
  */
case class PropMatchStrategy(property : String, isSubject : Boolean, override  val weight: Double, rdfTypes : List[String], findSimilarsActive : Boolean = true) extends Strategy {

  def matchEntityAndProperty(tuple: (String, String, String), entity : String): Boolean = {
    return tuple match {
      case (`entity`, `property`, o) if(isSubject) => true
      case (s, `property`, `entity`) if(!isSubject)=> true
      case _ => false
    }
  }

  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      if(other.statementsList.exists(matchEntityAndProperty(_, entity))) {
          featureMap += entity -> new Feature(property, FeatureType.sameProperty, 1, weight)
        }
      }
    return featureMap.toMap

  }

  override def findSimilars(): List[String] = {
    if(!findSimilarsActive) return Nil
    if(isSubject) return QueryFactory.findSubjectsOfTypeForProperty(property, rdfTypes)
    else return QueryFactory.findObjectsOfTypeForProperty(property, rdfTypes)
  }
  override def toString: String = {
    s"PropMatchStrategy for : $property isSubject=$isSubject, weight=$weight" + super.toString
  }

}
