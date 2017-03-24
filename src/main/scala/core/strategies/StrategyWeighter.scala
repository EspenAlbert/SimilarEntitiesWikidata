package core.strategies

import breeze.numerics.{log}
import core.globals.SimilarPropertyOntology.maxCountForProperties

/**
  * Created by espen on 22.03.17.
  */
object StrategyWeighter {
  var dateComparisonWeight = 0.3
  var valueMatchBoost = 5.0
  var directLinkBoost = 5.0
  var alternativeLinkNegative = -0.1

  def getWeight(strategy: Strategy) : Double = {
    strategy match {
      case a : ValueMatchStrategy => valueMatchBoost * logarithmicWeightForCount(a.dbCount)
      case b : PropertyMatchStrategy => logarithmicWeightForCount(b.dbCount)
      case c : AlternativeLinkStrategy => logarithmicWeightForCount(c.others.size) * alternativeLinkNegative
      case d : DirectLinkStrategy => directLinkBoost * logarithmicWeightForCount(d.others.size)
      case e : DateComparisonStrategy => dateComparisonWeight * logarithmicWeightForCount(e.dbCountProperty)
    }
  }

  def logarithmicWeightForCount(countForProperty: Int): Double = {
    return log(maxCountForProperties / countForProperty)
  }


}
