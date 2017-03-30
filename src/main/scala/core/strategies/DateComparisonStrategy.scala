package core.strategies

import breeze.numerics.abs
import core.feature.Feature
import core.globals.FeatureType
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF

import scala.collection.mutable
import core.globals.PrimitiveDatatype.getYearFromDateFormat
import similarityFinder.MyConfiguration
/**
  * Created by Espen on 11.11.2016.
  */
case class DateComparisonStrategy(property: String, value : String, dbCountProperty: Int ) extends Strategy{
  def otherHasSameTimeProperty(tuple : (String, String, String)) : Boolean = {
     tuple match {
      case (s, `property`, timevalue) => true
      case _ => false
    }
  }
  val valueInCorrectFormat = getYearFromDateFormat(value).getOrElse(99999)

  def calculateScore(otherTimeRaw: String) : Int = {
    val otherInStandard = getYearFromDateFormat(otherTimeRaw).getOrElse(-99999)
    val difference = abs(valueInCorrectFormat - otherInStandard)
    val score = MyConfiguration.windowForDateComparison - difference
    if(score > 0) return score
    if(score < -1 * MyConfiguration.windowForDateComparison) return -1 * MyConfiguration.windowForDateComparison //Makes them more different
    else return score
  }

  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      val statementsWithTimeProperty = other.statementsList.filter(otherHasSameTimeProperty(_))
      if(statementsWithTimeProperty.length > 0 ) {
        val score = calculateScore(statementsWithTimeProperty(0)._3)
        featureMap += entity -> new Feature(property, FeatureType.timeComparison, score, weight)
      }
    }
    return featureMap.toMap
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return Map() //Not worth it at the moment
  }
  val name = DateComparisonStrategy.name
  override def toString: String = {
    s"$name for : $property, weight=$weight" + super.toString
  }
}

object DateComparisonStrategy {
  val name = "DateComparisonStrategy"

}
