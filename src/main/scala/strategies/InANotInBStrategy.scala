package strategies
import feature.Feature
import globals.FeatureType
import rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
case class InANotInBStrategy(property: String, isSubject: Boolean, values : Iterable[String], override val weight : Double) extends Strategy{

  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      var count = 0
      if(isSubject) {
        for(a <- values) {
          if(other.statementsList.filter((s) => s._2 == property && s._1 == entity && s._3 == a).length < 1) {
            count += 1
          }

      } }else {
        for(a <- values) {
          if(other.statementsList.filter((s) => s._2 == property && s._1 == a && s._3 == entity).length < 1) count += 1
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
