package similarityFinder

import breeze.numerics.log
import core.globals.SimilarPropertyOntology
import core.strategies._

import scala.collection.mutable.ListBuffer

/**
  * Created by Espen on 14.11.2016.
  */
object MyConfiguration {


  var useMustHaveProperty: Boolean = false
  var numberOfComparableTypes = 10
  var filterOnRdfType = false
  val thresholdForBeingDescriptiveProperty = 3
  val verbose = true
  var useRdfType: Boolean = false
  var doScaling = false
  val globalInBNotInAActive = false
  val inANotInBActive = false
  val alActive = false
  var inANotInBBoost = -0.1
  var inBNotInABoost = 0.0
  var globalInBNotInABoost = -0.2
  val windowForDateComparison = 30
  var dateComparisonWeight = 0.3
  var valueMatchBoost = 5.0
  var directLinkBoost = 5.0
  var alternativeLinkNegative = -0.1
  var maximumWeightPropertyMatch = log(SimilarPropertyOntology.maxCountForProperties / 100)
  var thresholdCountCheapStrategy = 1000
  var thresholdCountStoreValueMatchCount = 10000

  def getConfigName(strategies : List[String]) : String = {
    val strategiesWithConfig = List(AggregatorStrategy.name, ValueMatchStrategy.name, DirectLinkStrategy.name, PropertyMatchStrategy.name, ExpandNodeStrategy.name)
    if(!strategies.forall(strategiesWithConfig.contains)) return ""
    var names = ListBuffer[String]()
    if(useRdfType) names += "UseRdfType"
    if(useMustHaveProperty) names += "UseMustHaveProperty"
    if(filterOnRdfType) names += "FilterOnRdfType"
    if(!strategies.forall(_== DirectLinkStrategy.name)) names += thresholdCountCheapStrategy.toString
    return names.mkString("-")
  }

}
