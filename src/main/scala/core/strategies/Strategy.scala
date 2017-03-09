package core.strategies

import breeze.numerics._
import core.feature.Feature
import core.rdf.GraphRDF

/**
  * Created by Espen on 09.11.2016.
  */
trait Strategy extends Ordered[Strategy]{
  def execute(otherEntities: List[GraphRDF]): Map[String, Feature]
  def findSimilars() : List[String]
  val weight : Double
  override def compare(that: Strategy): Int = {
    val otherHasHeigherWeight = that.weight - this.weight
    if(otherHasHeigherWeight > 0) return 1
    else if(otherHasHeigherWeight < 0) return -1
    return 0
  }
}
