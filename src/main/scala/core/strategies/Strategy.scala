package core.strategies

import breeze.numerics._
import core.feature.Feature
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF

/**
  * Created by Espen on 09.11.2016.
  */
trait Strategy extends Ordered[Strategy]{
  def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature]
  def findSimilars()(implicit knowledgeGraph: KnowledgeGraph) : Map[String, Feature]
//  val weight : Double
  def weight : Double = {
    return StrategyWeighter.getWeight(this)
  }
  override def compare(that: Strategy): Int = {
    val otherHasHeigherWeight = that.weight - this.weight
    if(otherHasHeigherWeight > 0) return 1
    else if(otherHasHeigherWeight < 0) return -1
    return 0
  }
}
