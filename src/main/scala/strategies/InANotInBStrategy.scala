package strategies
import feature.Feature
import globals.FeatureType
import rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
case class InANotInBStrategy(property: String, isSubject: Boolean, values : List[String], override val weight : Double) extends Strategy{

  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      var count = 0
      if(isSubject) {
        for(a <- other.statements) {
          a match {
            case (`entity`, `property`, value) if(!values.contains(value))=> count += 1//
            case _=> Unit
          }
        }
      } else {
        for(a <- other.statements) {
          a match {
            case (value, `property`, `entity`) if(!values.contains(value))=> count += 1
            case _ => Unit
          }
        }
      }
      if(count > 0) featureMap += entity -> new Feature(property, FeatureType.inANotInB, count, weight)
    }
    return featureMap.toMap
  }

  override def findSimilars(): List[String] = {
    return Nil
  }
}
