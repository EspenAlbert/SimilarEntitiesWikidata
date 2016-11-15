package main

import displayer.Displayer
import feature.Feature
import ranker.Ranker
import rdf.GraphRDF
import strategies.StrategyGenerator

import scala.collection.mutable

/**
  * Created by Espen on 09.11.2016.
  */
object SimilarityFinder {

  def findTopKSimilarTo(entity : String, topK : Int) : Unit = {
    val entityGraph = new GraphRDF(entity)
    val strategies = StrategyGenerator.generateStrategies(entityGraph)
    val sortedStrategies = strategies.sorted
    val otherEntities = mutable.Set[String]()
    var i = 0
    while (otherEntities.toList.length < 100) {
      val s = sortedStrategies(i)
//      otherEntities.add(find)
      otherEntities ++= s.findSimilars()
      i += 1
    }
    println(otherEntities)
    val otherEntitiesAsList = otherEntities.toList
    val featureMap = mutable.Map[String, List[Feature]]()
    for(s <- strategies) {
      val newFeatures: Map[String, List[Feature]] = s.execute(otherEntitiesAsList)
      println(newFeatures)
      featureMap ++= newFeatures
      println(featureMap.keys)
    }
    println(featureMap)
    val ranked = Ranker.getSortedOrder(featureMap.toMap)
    println(ranked)
    Displayer.displayResult(ranked, topK, entityGraph.entity)
//    implicit val system = ActorSystem("Sys")
//
//    import system.dispatcher
//
//    implicit val materializer = ActorMaterializer()
//
//    val source = Source.fromIterator(() => (strategies.toList ::: StrategyGenerator.generateInBNotInAStrategies(otherEntities.toList)).iterator).
//      map[Map[String, List[Feature]]]((s) => s.execute(otherEntitiesAsList)).
//      map[List[SimilarEntity]]((map) => Ranker.getSortedOrder(map)).
//      runForeach((result) => Displayer.displayResult(result, topK, entity)).
//      onComplete( _ => system.shutdown())
//    print("Hi")
//    val sourceValues = List("a", "b", "c", "d")
//    val mySource = Source.fromIterator( () => sourceValues.iterator).map[List[String]]((x) => List(x, x)).
//      filter((x) => x.exists(_.startsWith("a"))).runForeach(println).onComplete( _ => system.shutdown())

  }

}
