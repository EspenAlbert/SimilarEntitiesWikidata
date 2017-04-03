package similarityFinder

import breeze.numerics.log
import core.globals.SimilarPropertyOntology

/**
  * Created by Espen on 14.11.2016.
  */
object MyConfiguration {
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
  val maxCountForValueMatchesToFindSimlars = 10000
  def getConfigName : String = {
    if(useRdfType) return "UseRdfType"
    return ""
  }

}
