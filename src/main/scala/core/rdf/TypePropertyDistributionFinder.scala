package core.rdf

import breeze.linalg.{max, min}
import breeze.numerics.abs
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.{QueryFactoryJena, UpdateQueryFactory}

import scala.util.{Failure, Success}

/**
  * Created by espen on 04.05.17.
  */
object TypePropertyDistributionFinder {

  val thresholdForStoringPropertyDistributionsLocally = 1000

    def propertyDistributionChange(distributionA :Map[String, (Double, Int, Int)], distributionB: Map[String, (Double, Int, Int)]): Double = {
      val totalChange = {
        for {
          (property, (importanceRatio, _, _)) <- distributionA
          (otherImportanceRatio: Double, _, _) <- distributionB.get(property)
          change = max(abs(importanceRatio - otherImportanceRatio), importanceRatio)
        } yield change
      }.sum
      val maxChange = distributionA.foldLeft(0d)(_ + _._2._1)
      return totalChange / maxChange

    }
  def propertyDistributionOverlap(distributionA :Map[String, (Double, Int, Int)], distributionB: Map[String, (Double, Int, Int)]): Double = {
    val abOverlap: Double = calculateOverlap(distributionA, distributionB)
    val baOverlap: Double = calculateOverlap(distributionB, distributionA)
    return min(abOverlap, baOverlap)

  }

  private def calculateOverlap(distributionA: Map[String, (Double, Int, Int)], distributionB: Map[String, (Double, Int, Int)]): Double = {
    val totalOverlaps = {
      for {
        (property, (_, _, _)) <- distributionA
        (_) <- distributionB.get(property)
      } yield 1
    }.sum
    val maxOverlap = distributionA.keySet.size
    return totalOverlaps.toDouble / maxOverlap
  }

  def propertyDistributionIgnoreRareness(typeEntity: String)(implicit knowledgeGraph: KnowledgeGraph) : Map[String, (Double, Int, Int)] = {
    val globalCount : Int= TypeCounter.findGlobalCountOfEntitiesOfType(typeEntity).getOrElse(throw new Exception(s"Failed to find global count for $typeEntity"))
    if(globalCount > thresholdForStoringPropertyDistributionsLocally) {
      QueryFactoryJena.allPropertyDistributionsLocally(typeEntity) match {
        case list if list.nonEmpty => return list.map{
          case (property, Success(domainCount), Success(rangeCount), importanceRatio) => property -> ( importanceRatio, domainCount, rangeCount)
          case (property, Success(domainCount), Failure(_), importanceRatio) => property-> ( importanceRatio, domainCount, 0)
          case (property, Failure(_), Success(rangeCount), importanceRatio) => property-> (importanceRatio, 0, rangeCount)
          case _ => throw new Exception(s"both rangeCount and domain count was not found locally for $typeEntity")
        }.toMap
        case _ => Unit
      }

    }
    val domainProps = QueryFactoryJena.propertiesWhereDomainHasType(typeEntity).map((_, true))
    val rangeProps = QueryFactoryJena.propertiesWhereRangeHasType(typeEntity).map((_, false))
    val propertiesAndIsSubject: List[(String,Boolean)] = domainProps ++ rangeProps
    val distribution = {for{
      (property : String, (usedInDomain, usedInRange)) <- propertiesAndIsSubject.groupBy(_._1).map{
        case (property, (list)) => ((property), (list.exists(_._2), list.exists(!_._2)))
      }
      domainCount : Int = if(usedInDomain) QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, true, property) else 0
      rangeCount : Int = if(usedInRange) QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, false, property) else 0
      if (domainCount + rangeCount)  > globalCount * 0.1
      importanceRatio = (domainCount + rangeCount).toDouble / globalCount
    }yield property -> (importanceRatio, domainCount, rangeCount)}.toMap
    if(globalCount > thresholdForStoringPropertyDistributionsLocally) {
    UpdateQueryFactory.addPropertyDistribution(typeEntity, distribution)}
    return distribution
  }

}
