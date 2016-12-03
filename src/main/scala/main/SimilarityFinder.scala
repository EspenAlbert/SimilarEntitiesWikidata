package main

import displayer.Displayer
import feature.Feature
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
//      otherEntities.add(find)
      otherEntities ++= s.findSimilars()
      i += 1
    }
    val otherEntitiesAsGraphs = otherEntities.map((s) => new GraphRDF(s)).toList
    val featureMap = mutable.Map[String, ListBuffer[Feature]]()
    for(s <- strategies) {
      val newFeatures: Map[String, Feature] = s.execute(otherEntitiesAsGraphs)
      addFeaturesToMap(featureMap, newFeatures)
    }
    val ranked = Ranker.getSortedOrder(featureMap.toMap)
    Displayer.displayResult(ranked, topK, entityGraph.entity)
    return ranked
//    implicit val system = ActorSystem("Sys")
//
//    import system.dispatcher
//
//    implicit val materializer = ActorMaterializer()
//
//    val source = Source.fromIterator(() => (strategies.toList ::: StrategyGenerator.generateInBNotInAStrategies(otherEntities.toList)).iterator).
//      map[Map[String, List[Feature]]]((s) => s.execute(otherEntitiesAsGraphs)).
//      map[List[SimilarEntity]]((map) => Ranker.getSortedOrder(map)).
//      runForeach((result) => Displayer.displayResult(result, topK, entity)).
//      onComplete( _ => system.shutdown())
//    print("Hi")
//    val sourceValues = List("a", "b", "c", "d")
//    val mySource = Source.fromIterator( () => sourceValues.iterator).map[List[String]]((x) => List(x, x)).
//      filter((x) => x.exists(_.startsWith("a"))).runForeach(println).onComplete( _ => system.shutdown())

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
