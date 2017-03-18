package similarityFinder

import core.feature.Feature
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF
import core.strategies.{Strategy, StrategyGenerator}
import similarityFinder.displayer.Displayer
import similarityFinder.ranker.{Ranker, SimilarEntity}

import scala.collection.mutable.ListBuffer
import scala.collection.{Set, mutable}

/**
  * Created by Espen on 09.11.2016.
  */
object SimilarityFinder {
  val SKIP_PRINTING = true
  val USE_PRUNING = true
  def findTopKSimilarTo(entity : String, topK : Int)(implicit knowledgeGraph: KnowledgeGraph) : List[SimilarEntity] = {
    val (entityGraph: GraphRDF, sortedStrategies: Array[Strategy]) = findGraphAndStrategiesForEntity(entity)
    return findSimilarToEntityWithStrategies(topK, entityGraph, sortedStrategies)
  }

  def findGraphAndStrategiesForEntity(entity: String)(implicit knowledgeGraph: KnowledgeGraph) = {
    val entityGraph = new GraphRDF(entity)
    val strategies = StrategyGenerator.generateStrategies(entityGraph)
    val sortedStrategies = strategies.sorted
    (entityGraph, sortedStrategies)
  }


  def findSimilarToEntityWithStrategies(topK: Int, entityGraph: GraphRDF, sortedStrategies: Array[Strategy])(implicit knowledgeGraph: KnowledgeGraph): List[SimilarEntity] = {
    val otherEntities: Set[String] = if(!USE_PRUNING) findSimilarsUnranked(sortedStrategies) else findSimilarsUnrankedWithPruning(sortedStrategies)
    println("Similars found : ", otherEntities.size)
    return calculateAndRankSimilarity(topK, entityGraph, sortedStrategies, otherEntities)
  }

  def calculateAndRankSimilarity(topK: Int, entityGraph: GraphRDF, sortedStrategies: Array[Strategy], otherEntities: Set[String])(implicit knowledgeGraph: KnowledgeGraph): List[SimilarEntity] = {
    val otherEntitiesAsGraphs = otherEntities.map((s) => new GraphRDF(s)).toList
    val featureMap = mutable.Map[String, ListBuffer[Feature]]()
    for (s <- sortedStrategies) {
      val newFeatures: Map[String, Feature] = s.execute(otherEntitiesAsGraphs)
      addFeaturesToMap(featureMap, newFeatures)
    }
    if (MyConfiguration.doScaling) {

    }
    val ranked = if (MyConfiguration.doScaling) Ranker.getSortedOrderScaled(featureMap.toMap, otherEntitiesAsGraphs, entityGraph) else Ranker.getSortedOrder(featureMap.toMap)

    if (!SKIP_PRINTING) Displayer.displayResult(ranked, topK, entityGraph.entity)
    return ranked
  }

  def findSimilarsUnranked(sortedStrategies: Array[Strategy])(implicit knowledgeGraph: KnowledgeGraph): mutable.Set[String] = {
    val otherEntities = mutable.Set[String]()
    var i = 0
    while (otherEntities.toList.length < 1000 && i < sortedStrategies.length) {
      val s = sortedStrategies(i)
      println("Find similars: " + s)
      otherEntities ++= s.findSimilars()
      i += 1
    }
    return otherEntities
  }
  def findSimilarsUnrankedWithPruning(sortedStrategies: Array[Strategy])(implicit knowledgeGraph: KnowledgeGraph): Set[String] = {
    val otherEntities = mutable.Map[String, Int]()
    var strategyNumber = 0
    while (!isFinishedWithPruning(sortedStrategies, otherEntities, strategyNumber)) {
      val s = sortedStrategies(strategyNumber)
      println("Find similars: " + s)
      for(e  <- s.findSimilars()) {
        if(otherEntities.contains(e)) {
          otherEntities(e) += 1
        }
        else otherEntities += (e -> 1)
      }
      strategyNumber += 1
    }
    return getEntitiesWithMin2Strategies(otherEntities).toSet
  }
  private def isFinishedWithPruning(sortedStrategies: Array[Strategy], otherEntities: mutable.Map[String, Int], strategyNumber: Int) : Boolean = {
    if(strategyNumber == sortedStrategies.length -1) return true
    if(otherEntities.size < 1000) return false
    else {
      println("bonus strategy")
      val filtered = getEntitiesWithMin2Strategies(otherEntities)
      if(filtered.size > 1000) return true
      else return false
//
    }
  }
  val MAX_P_ITERATIONS = 66

//  def sortMapOfStringInt(otherEntities: mutable.Map[String, Int]): List[String] = {
//    class SimilarE(val name : String, val count : Int) extends Ordered[SimilarE] {
//      override def compare(that: SimilarE): Int = {
//        return this.count - that.count
//      }
//    }
////    val sortedSimilarE = otherEntities.map((s) => new SimilarE(s._1, s._2)).toList.sorted
////    println("Sorted Similar 1k has #core.strategies = ", sortedSimilarE(999).count)
////    println("Sorted Similar 2 has #core.strategies = ", sortedSimilarE(1).count)
//    print("Similars with count > 2", otherEntities.filter((s) => s._2 > 1).size)
////    return sortedSimilarE.map((s) => (s.name -> s.count))
////    return sortedSimilarE.map(_.name)
//    return otherEntities.filter((s) => s._2 > 1).keys.toList
//  }

  private def getEntitiesWithMin2Strategies(otherEntities: mutable.Map[String, Int]): Iterable[String] = {
      return otherEntities.filter((s) => s._2 > 1).keys
//    return sortedSimilars
//    return otherEntities.filter((tuple) => tuple._2 >= MINIMUM_STRATEGIES).map(_._1)
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
