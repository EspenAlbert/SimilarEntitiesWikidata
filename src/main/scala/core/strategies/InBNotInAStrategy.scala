package core.strategies

import core.feature.Feature
import core.globals.FeatureType
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
case class InBNotInAStrategy(val property: String, isSubject: Boolean, val valuesInA : List[String], override val weight : Double) extends Strategy{

  def statementIsCorrectPropertyAndADoesNotHaveValue(statement : (String, String, String)) : Boolean = {
  statement match {
    case (s, `property`, o) if(!valuesInA.contains(o)) => true
    case _ => false
  }
}

  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      var count = 0
      val propertiesOther = if (isSubject) other.getProperties(s = true) else other.getProperties(o = true)
      if (propertiesOther.contains(property)) {
        val valuesNotInA = other.statementsList.filter(statementIsCorrectPropertyAndADoesNotHaveValue(_))
        val valuesNotInALength = valuesNotInA.length
        if (valuesNotInALength > 0) {
          featureMap += entity -> new Feature(property, FeatureType.inBNotInA, valuesNotInALength, weight)
        }
      }
    }
    return featureMap.toMap
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): List[String] = {
    return Nil
  }
}
