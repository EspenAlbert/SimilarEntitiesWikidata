package strategies

import breeze.numerics.abs
import feature.Feature
import globals.{FeatureType, MyConfiguration}
import rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  *///AlternativeLinkStrategy(property, Set() ++ filteredRange, true, logarithmicWeight(filteredRange.length))
case class DateComparisonStrategy(property: String, value : String, override val weight : Double ) extends Strategy{
  def otherHasSameTimeProperty(tuple : (String, String, String)) : Boolean = {
     tuple match {
      case (s, `property`, timevalue) => true
      case _ => false
    }
  }
  val valueInCorrectFormat = DateComparisonStrategy.getStandardDateFormat(value)

  def calculateScore(otherTimeRaw: String) : Int = {
    val otherInStandard = DateComparisonStrategy.getStandardDateFormat(otherTimeRaw)
    val difference = abs(valueInCorrectFormat - otherInStandard)
    val score = MyConfiguration.windowForDateComparison - difference
    if(score > 0) return score
    if(score < -1 * MyConfiguration.windowForDateComparison) return -1 * MyConfiguration.windowForDateComparison //Makes them more different
    else return score
  }

  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = {
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

  override def findSimilars(): List[String] = {
    return Nil //Not worth it at the moment
  }
}
object DateComparisonStrategy {
  def getStandardDateFormat(stringDateRaw : String) : Int = {
    stringDateRaw.trim() match {
      case x if x endsWith("gYearMonth>") => return x.substring(1, 5).toInt
      case x if x endsWith("#date>") => return x.substring(1, 5).toInt
      case x if x endsWith("gYear>") => return x.substring(1, 5).toInt
      case x => println(s"dont know how to convert $x"); return 999999
    }
    println(s"Raw string received $stringDateRaw")
    return 2
  }

}
