package strategies

import breeze.linalg.min
import breeze.numerics.log
import feature.Feature
import globals.{FeatureType, MyConfiguration, MyDatasets}
import globals.SimilarPropertyOntology.maxCountForProperties
import query.specific.QueryFactory
import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.SimpleRDF

import scala.collection.mutable
/**
  * Created by Espen on 10.11.2016.
  */
case class PropMatchStrategy(property : String, isSubject : Boolean) extends Strategy {
//  new SimpleRDF()
  def partiallyAppliedStatement(otherEntity : String) : SimpleRDF =  {//TODO: Must be same type!
    if(isSubject) return new SimpleRDF(new StaticQueryVariable(otherEntity), new StaticQueryVariable(property), new DynamicQueryVariable("o", false)) else
      return new SimpleRDF(p = new StaticQueryVariable(property), o = new StaticQueryVariable(otherEntity))
  }

  override def execute(otherEntities: List[String]): Map[String, List[Feature]] = {
    println(s"executing : $property ....")
    val featureMap = mutable.Map[String, List[Feature]]()
    for (other <- otherEntities) {
      val otherHasProperty = QueryFactory.ask(partiallyAppliedStatement(other))
      if (otherHasProperty) {
        featureMap(other) = featureMap.get(other) match {
          case Some(list) => list ::: List(new Feature(property, FeatureType.sameProperty, 1, getWeight()))
          case None => List(new Feature(property, FeatureType.sameProperty, 1, getWeight()))
        }
      }
    }
    return featureMap.toMap

  }

  override def findSimilars(): List[String] = {
    QueryFactory.dataset = MyDatasets.Wikidata
    if(isSubject) return QueryFactory.findSubjectsWithProperty(property) else return QueryFactory.findObjectsWithProperty(property)
  }
  def logarithmicWeight(countForProperty : Int) : Double = {
    return log(maxCountForProperties.toString.toInt / countForProperty)
  }
  override def weightCalculator() : Double  = if(isSubject) return logarithmicWeight(QueryFactory.findDomainCount(property)) else return logarithmicWeight(QueryFactory.findRangeCount(property))

  def getWeight(f : () => Double = weightCalculator) : Double= {
    val tooCommonProperties = Set("P31", "P17", "P131")

    if(tooCommonProperties.contains(property.substring(property.indexOf("P")))) return 0
    return min(f(), MyConfiguration.maximumWeightPropertyMatch)
  }

}
