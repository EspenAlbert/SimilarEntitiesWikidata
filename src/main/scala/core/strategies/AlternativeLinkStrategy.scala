package core.strategies
import core.feature.Feature
import core.globals.FeatureType
import core.rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  *///AlternativeLinkStrategy(property, Set() ++ filteredRange, true, logarithmicWeight(filteredRange.length))
case class AlternativeLinkStrategy(property: String, others : Set[String], isSubject: Boolean, override val weight : Double) extends Strategy{

  def isOppositeOfIsSubjectForProperty(tuple : (String, String, String), entity : String) : Boolean = {
    if(isSubject) tuple match {
      case (s, `property`, `entity`) => true
      case _ => false
    } else tuple match {
      case (`entity`, `property`, o) => true
      case _ => false
    }
  }
  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      val alternativeLinks = other.statementsList.filter(isOppositeOfIsSubjectForProperty(_, entity))
      val minus1 = if(others.contains(entity)) 1 else 0
      if((alternativeLinks.length - minus1) > 0) featureMap += entity -> new Feature(property, FeatureType.alternativeMatch, alternativeLinks.length - 1, weight)
    }
    return featureMap.toMap
  }

  override def findSimilars(): List[String] = throw new Exception("This strategy is not suited to find similars !!")

}
