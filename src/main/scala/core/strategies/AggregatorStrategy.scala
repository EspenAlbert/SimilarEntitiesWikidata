package core.strategies

import core.feature.{Feature, ValueMatchFeature}
import core.globals.FeatureType
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.QueryFactory
import core.rdf.GraphRDF
import similarityFinder.MyConfiguration

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
case class AggregatorStrategy(entity : String, property: String, isSubject: Boolean, values : List[String], rdfTypes : List[String]) extends Strategy{

  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return Map()
//    println(s"Finding similars for property (expensive strategy): $property")

//    val featureMap = mutable.Map[String, Feature]()
//    for (other <- otherEntities) {
//      val entity: String = other.entity
//      if(isSubject) {
//        for(a <- other.statementsList) {
//          a match {
//            case (`entity`, `property`, `value`) => featureMap += entity -> new ValueMatchFeature(property, FeatureType.valueMatch, 1, weight, value)
//            case _=> Unit
//          }
//        }
//      } else {
//        for(a <- other.statementsList) {
//          a match {
//            case (`value`, `property`, `entity`) => featureMap += entity -> new ValueMatchFeature(property, FeatureType.valueMatch, 1, weight, value)
//            case _ => Unit
//          }
//        }
//      }
//    }
//    return featureMap.toMap
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val propertyValueCounts = QueryFactory.findPropertyObjectPairsCountsForSubjectsHavingEntityAsObjectForProperty(property, entity).filterNot(_._2 == entity)
    val similars = mutable.HashSet[String]()
    for{
      (subjectProperty, objectValue, count) <- propertyValueCounts
      vmCount <-StrategyFactory.valueIsAPotentialValueMatchFindCount(objectValue, subjectProperty, true)
      thresholdCount <- (2 to count+2).find(c => (vmCount / c) < MyConfiguration.thresholdCountCheapStrategy)
    }{
      similars ++= QueryFactory.findObjectsHavingMoreThanXStatementsOfPropertyWhereSubjectsAgainHaveObjectValue(thresholdCount, property, (subjectProperty, objectValue))
      //TODO: Add switch on and off for type restriction
    }
    return similars.map(_ -> new Feature(property, FeatureType.aggregateFeature, 1, weight)).toMap


    //    println(s"Finding similars for property (cheap strategy) : $property")
//    val entities = MyConfiguration.useRdfType match {
//      case true => if (isSubject) QueryFactory.subjectsOfTypeWithPropertyAndValue(property, value, rdfTypes) else
//        QueryFactory.objectsOfTypeWithPropertyAndSubject(property, value, rdfTypes)
//      case false => if (isSubject) QueryFactory.subjectsWithPropertyAndValue(property, value) else
//        QueryFactory.objectsWithPropertyAndSubject(property, value)
//    }
//    return entities.map(e=> e-> new ValueMatchFeature(property, FeatureType.valueMatch, 1, weight, value)).toMap
  }
  val name = AggregatorStrategy.name
  override def toString: String = {
    s"$name for : $property isSubject=$isSubject values=$values, weight=$weight" + super.toString
  }

}

object AggregatorStrategy {
  val name = "AggregatorStrategy"
}



