package globals

import breeze.numerics.log

/**
  * Created by Espen on 14.11.2016.
  */
object MyConfiguration {
  val maximumWeightPropertyMatch = log(SimilarPropertyOntology.maxCountForProperties.toString.toInt / 100)
}
