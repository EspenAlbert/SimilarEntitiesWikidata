package globals

import breeze.numerics.log

/**
  * Created by Espen on 14.11.2016.
  */
object MyConfiguration {


  val valueMatchBoost = 2
  val directLinkBoost = 5
  val alternativeLinkNegative = -0.1
  val maximumWeightPropertyMatch = log(SimilarPropertyOntology.maxCountForProperties.toString.toInt / 100)
}
