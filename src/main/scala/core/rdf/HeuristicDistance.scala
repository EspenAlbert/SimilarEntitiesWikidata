package core.rdf

import core.globals.KnowledgeGraphs
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.{AskQuery, QueryFactoryJena}

import scala.collection.mutable
import scala.util.Try

/**
  * Created by espen on 03.05.17.
  */
object HeuristicDistance {

  def findClosestIndependentParentAndDistance(entityType: String, threshold: Int)(implicit knowledgeGraphs: KnowledgeGraph) : Option[(String, Int)] = {
    orderedHeuristicDistanceToParents(entityType)
      .collectFirst {
        case (parent, d) if TypeCounter.findGlobalCountOfEntitiesOfType(parent).get > threshold && d > 0 => (parent, d)
      }
  }


  def orderedHeuristicDistanceToChildren(entityType : String)(implicit knowledgeGraph: KnowledgeGraph): Stream[(String, Int)] = {
    createStreamDownwards(entityType, Nil,0)

  }

  def createStreamDownwards(startEntity : String, remainingEntities : Iterable[String], currentLevel: Int)(implicit knowledgeGraph: KnowledgeGraph): Stream[(String, Int)] = {
    remainingEntities match {
      case Nil => {
        val nextEntities = QueryFactoryJena.childrenOfEntityXStepsAway(startEntity,currentLevel+1)
        if(nextEntities.isEmpty || currentLevel==getMaximumDepth) Stream.empty
        else (nextEntities.head, currentLevel+1) #:: createStreamDownwards(startEntity, nextEntities.tail, currentLevel+1)
      }
      case head::tail => (head, currentLevel) #:: createStreamDownwards(startEntity, tail, currentLevel)
    }
  }

  def orderedHeuristicDistanceToParents(entityType: String)(implicit knowledgeGraph: KnowledgeGraph): Stream[(String, Int)] = {
    return createStreamUpwards(entityType, Nil, 0)
  }
  def getMaximumDepth(implicit knowledgeGraph: KnowledgeGraph) : Int = knowledgeGraph match {
    case KnowledgeGraphs.wikidata => 15
    case KnowledgeGraphs.dbPedia => 7
  }

  def createStreamUpwards(startEntity: String, remainingEntities : Iterable[String], currentLevel: Int)(implicit knowledgeGraph: KnowledgeGraph): Stream[(String, Int)] = {
    remainingEntities match {
      case Nil => {
        val nextEntities = QueryFactoryJena.parentToEntityXStepsAway(startEntity,currentLevel+1)
        if(nextEntities.isEmpty || currentLevel==getMaximumDepth) Stream.empty
        else (nextEntities.head, currentLevel+1) #:: createStreamUpwards(startEntity, nextEntities.tail, currentLevel+1)
      }
      case head::tail => (head, currentLevel) #:: createStreamUpwards(startEntity, tail, currentLevel)
    }
  }



  def determineNextStream(streamA: Stream[(String, Int)], streamB: Stream[(String, Int)]): Option[Boolean] = {
    (streamA, streamB) match {
      case (Stream.Empty, Stream.Empty) => None
      case (Stream.Empty, _) => Some(false)
      case (_, Stream.Empty) => Some(true)
      case (s1#::_, s2#::_) => Some(s1._2 <= s2._2)
    }
  }

  def findHeuristicDistanceImproved(a: String, b: String)(implicit knowledgeGraph: KnowledgeGraph) : Option[Int] = {
    if(a == b) return Some(0)
    var streamA = orderedHeuristicDistanceToParents(a)
    var streamB = orderedHeuristicDistanceToParents(b)
    val distanceToParentsA = mutable.HashMap[String, Int](a -> 0).withDefaultValue(-1)
    val distanceToParentsB = mutable.HashMap[String, Int](b -> 0).withDefaultValue(-1)
    var nextElementIsA : Boolean= determineNextStream(streamA, streamB) match {
      case Some(b) => b
      case None => return None
    }
    while(true) {
      val (parent, distance) =  if(nextElementIsA) streamA.head else streamB.head
      if(nextElementIsA) {streamA = streamA.tail; distanceToParentsA.update(parent, distance) }else {streamB = streamB.tail; distanceToParentsB.update(parent, distance)}
      val otherDistanceToParent = if(nextElementIsA) distanceToParentsB(parent) else distanceToParentsA(parent)
      if(otherDistanceToParent > -1) {
        val heuristicDistance = distance + otherDistanceToParent
        return Some(heuristicDistance)
      }
      determineNextStream(streamA, streamB) match {
        case Some(b) => nextElementIsA = b
        case None => return None
      }
    }
    return None
  }

}
