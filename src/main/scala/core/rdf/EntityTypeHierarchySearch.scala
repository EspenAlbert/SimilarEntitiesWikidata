package core.rdf

import breeze.linalg.max
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.{AskQuery, QueryFactoryJena}
import core.testData.WikidataFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by espen on 25.05.17.
  */
case class EntityTypeHierarchySearch(entityType : String, upDistanceMax: Int, downDistanceMax: Int, noNesting: Boolean, addConditions: (String=>Boolean)*)(implicit knowledgeGraph: KnowledgeGraph) {
  val upstream = HeuristicDistance.orderedHeuristicDistanceToParents(entityType)
  val downstream = HeuristicDistance.orderedHeuristicDistanceToChildren(entityType)
  val notCompatibleEntityTypes = mutable.HashSet[String]()

  def findAllEntityTypes() : List[String] = {
//    if(entityType == WikidataFactory.human) return entityType::Nil
    (entityType::exploreStream(upstream, upDistanceMax, goingUp = true, isNested= false) ::: exploreStream(downstream, downDistanceMax, goingUp = false, isNested= false)).distinct
  }

  val similarEntityTypes = mutable.HashSet[String]()

  private def exploreStream(stream: Stream[(String, Int)], maxSteps : Int, goingUp : Boolean, isNested : Boolean): List[String] = {
    val streamIterator = stream.iterator
    var currentDistance = 1
    val illegalPrecessors = ListBuffer[String]()
    var layerHasANode = false
    while (streamIterator.hasNext) {
      val (entityTypeId,entityTypeDistance)  = streamIterator.next()
      if (entityTypeDistance != currentDistance) {
        if (entityTypeDistance > maxSteps || !layerHasANode) return similarEntityTypes.toList
        else {
          currentDistance += 1
          layerHasANode = false
        }
      }
      if (!similarEntityTypes.contains(entityTypeId) && addConditions.forall(condition => condition(entityTypeId)) && !cameFromIllegalPrecessor(entityTypeId, illegalPrecessors, goingUp, entityTypeDistance)) {
        layerHasANode = true
//        if(!similarEntityTypes.contains(entityTypeId)) {
        similarEntityTypes.add(entityTypeId)
//        }
        if(!noNesting && !isNested && entityTypeDistance != maxSteps) {
          val nestedExplorationGoingUp = !goingUp
          val nestedStream = if(nestedExplorationGoingUp) HeuristicDistance.orderedHeuristicDistanceToParents(entityTypeId) else HeuristicDistance.orderedHeuristicDistanceToChildren(entityTypeId)
//          val nestedSimilarEntities = exploreStream(nestedStream, entityTypeDistance, nestedExplorationGoingUp, isNested=true)//.filterNot(similarEntityTypes.contains)
          exploreStream(nestedStream, entityTypeDistance, nestedExplorationGoingUp, isNested=true)//.filterNot(similarEntityTypes.contains)
//          similarEntityTypes.append(nestedSimilarEntities:_*)
        }
      }
      else {
        illegalPrecessors.append(entityTypeId)
      }
    }
    return similarEntityTypes.toList
  }
  def cameFromIllegalPrecessor(nextEntity: String, illegalPrecessors: ListBuffer[String], goingUp: Boolean, stepsAwayFromRoot: Int): Boolean = {
    if(illegalPrecessors.isEmpty || stepsAwayFromRoot < 2) return false
    if(goingUp) {
      val childrenOfNext = QueryFactoryJena.childrenOfEntityXStepsAway(nextEntity, 1)
      childrenOfNext.exists(illegalPrecessors.contains)
    }
    else {
      val parentsOfNext = QueryFactoryJena.parentToEntityXStepsAway(nextEntity,1)
      parentsOfNext.exists(illegalPrecessors.contains)
    }
  }

}

object EntityTypeHierarchySearch {
  def addEntityTypeIfCountEntityTypeLowerThan(threshold : Int)(entity : String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    TypeCounter.findGlobalCountOfEntitiesOfType(entity) match {
      case Some(entityCount) if entityCount < threshold && entityCount> 0=> true
      case _ => false
    }
  }
  def addEntityTypeIfPropertyDistributionSimilar(queryEntity: String, threshold: Double)(entity: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    TypeCounter.findGlobalCountOfEntitiesOfType(entity) match {
      case Some(entityCount) if entityCount < 100 => return true
      case None=> return true
      case _=> Unit
    }
    val propertyDistributionQueryEntity = TypePropertyDistributionFinder.propertyDistributionIgnoreRareness(queryEntity)
    val overlap = TypePropertyDistributionFinder.propertyDistributionOverlap(propertyDistributionQueryEntity, entity, (a: Double, b: Double) => (a + b) / 2)
    println(s"$overlap overlap between: $queryEntity - $entity")
    overlap > threshold
  }
}
