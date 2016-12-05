package globals

import breeze.numerics.log

/**
  * Created by Espen on 14.11.2016.
  */
object MyConfiguration {


  var doScaling = true


  var inANotInBBoost = -0.3
  var inBNotInABoost = -0.1
  val windowForDateComparison = 30
  var dateComparisonWeight = 0.3
  var valueMatchBoost = 2.0
  var directLinkBoost = 5.0
  var alternativeLinkNegative = -0.1
  var maximumWeightPropertyMatch = log(SimilarPropertyOntology.maxCountForProperties.toString.toInt / 100)
}
