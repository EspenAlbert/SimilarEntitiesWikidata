package strategies

import breeze.numerics._
import feature.Feature

/**
  * Created by Espen on 09.11.2016.
  */
trait Strategy extends Ordered[Strategy]{
  def execute(otherEntities : List[String]) : Map[String, List[Feature]]
  def findSimilars()
  val weight : Double
  override def compare(that: Strategy): Int = {
    return floor(that.weight - this.weight).toInt
  }
}
