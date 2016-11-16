package strategies

import breeze.numerics._
import feature.Feature
import rdf.GraphRDF

/**
  * Created by Espen on 09.11.2016.
  */
trait Strategy extends Ordered[Strategy]{
  def execute(otherEntities: List[GraphRDF]): Map[String, Feature]
  def findSimilars() : List[String]
  val weight : Double
  override def compare(that: Strategy): Int = {
    return floor(that.weight - this.weight).toInt
  }
}
