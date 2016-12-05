package main

import displayer.Displayer
import feature.Feature
import globals.MyConfiguration
import ranker.{Ranker, SimilarEntity}
import rdf.GraphRDF
import strategies.StrategyGenerator

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Espen on 09.11.2016.
  */
object SimilarityFinder {

  def findTopKSimilarTo(entity : String, topK : Int) : List[SimilarEntity] = {
    val entityGraph = new GraphRDF(entity)
    val strategies = StrategyGenerator.generateStrategies(entityGraph)
    val sortedStrategies = strategies.sorted
    val otherEntities = mutable.Set[String]()
    var i = 0
    while (otherEntities.toList.length < 1000) {
      val s = sortedStrategies(i)
      otherEntities ++= s.findSimilars()
      i += 1
    }
    val otherEntitiesAsGraphs = otherEntities.map((s) => new GraphRDF(s)).toList
    val featureMap = mutable.Map[String, ListBuffer[Feature]]()
    for(s <- strategies) {
      val newFeatures: Map[String, Feature] = s.execute(otherEntitiesAsGraphs)
      addFeaturesToMap(featureMap, newFeatures)
    }
    if(MyConfiguration.doScaling) {

    }
    val ranked = if(MyConfiguration.doScaling) Ranker.getSortedOrderScaled(featureMap.toMap, otherEntitiesAsGraphs, entityGraph) else Ranker.getSortedOrder(featureMap.toMap)

    Displayer.displayResult(ranked, topK, entityGraph.entity)
    return ranked
  }

  def addFeaturesToMap(featureMap: mutable.Map[String, ListBuffer[Feature]], newFeatures: Map[String, Feature]) = {
    for ((s, f) <- newFeatures) {
      featureMap.get(s) match {
        case Some(l) => l += f
        case None => featureMap += s -> ListBuffer(f)
      }
    }
  }
}
