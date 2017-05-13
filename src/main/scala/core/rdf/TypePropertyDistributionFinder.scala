package core.rdf

import breeze.linalg.{max, min}
import breeze.numerics.abs
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.SimilarPropertyOntology
import core.query.specific.{DatasetInferrer, QueryFactoryJena, UpdateQueryFactory}

import scala.util.{Failure, Success}

/**
  * Created by espen on 04.05.17.
  */
object TypePropertyDistributionFinder {

  val thresholdForStoringPropertyDistributionsLocally = 1000

//    def propertyDistributionChange(distributionA :Map[String, (Double, Int, Double, Int)], distributionB: Map[String, (Double, Int, Double, Int)]): Double = {
//      val totalChange = {
//        for {
//          (property, (importanceRatio, _, _)) <- distributionA
//          (otherImportanceRatio: Double, _, _) <- distributionB.get(property)
//          change = max(abs(importanceRatio - otherImportanceRatio), importanceRatio)
//        } yield change
//      }.sum
//      val maxChange = distributionA.foldLeft(0d)(_ + _._2._1)
//      return totalChange / maxChange
//    }
  def propertyDistributionOverlap(distributionA :Map[String, (Double, Int, Double, Int)], distributionB: Map[String, (Double, Int, Double,Int)]): Double = {
    val abOverlap: Double = calculateOverlap(distributionA, distributionB)
    val baOverlap: Double = calculateOverlap(distributionB, distributionA)
    return min(abOverlap, baOverlap)

  }

  private def calculateOverlap(distributionA: Map[String, (Double, Int, Double,Int)], distributionB: Map[String, (Double, Int,Double, Int)]): Double = {
    val totalOverlaps = {
      for {
        (property, _) <- distributionA
        (_) <- distributionB.get(property)
      } yield 1
    }.sum
    val maxOverlap = distributionA.keySet.size
    return totalOverlaps.toDouble / maxOverlap
  }

  val requirementForStoringPropertyInDistribution = 0.0001
  var deletePropertyDistribution = false

  def propertyDistributionIgnoreRareness(typeEntity: String)(implicit knowledgeGraph: KnowledgeGraph) : Map[String, (Double, Int, Double, Int)] = {
    if(deletePropertyDistribution) {
      UpdateQueryFactory.cleanDatasetWhere(DatasetInferrer.getDataset(s"?s <${SimilarPropertyOntology.spo}> ?o"), s"<$typeEntity> <${SimilarPropertyOntology.propertyDistributionNode}> ?n")
    }
    val globalCount : Int= TypeCounter.findGlobalCountOfEntitiesOfType(typeEntity).getOrElse(throw new Exception(s"Failed to find global count for $typeEntity"))
    if(globalCount > thresholdForStoringPropertyDistributionsLocally) {
      QueryFactoryJena.allPropertyDistributionsLocally(typeEntity) match {
        case list if list.nonEmpty => return list.map{
          case (property, Success(importanceRatioDomain), Success(domainCount), Success(importanceRatioRange),Success(rangeCount)) => property -> ( importanceRatioDomain, domainCount, importanceRatioRange, rangeCount)
          case (property, Success(importanceRatioDomain), Success(domainCount), Failure(_),Failure(_)) => property -> ( importanceRatioDomain, domainCount, 0d, 0)
          case (property, Failure(_), Failure(_), Success(importanceRatioRange),Success(rangeCount)) => property -> ( 0d, 0, importanceRatioRange, rangeCount)
//          case (property, Failure(_), Failure(_),Failure(_) ,Success(rangeCount)) => property -> ( 0d, 0, rangeCount.toDouble / globalCount, rangeCount)
//          case (property, Failure(_), Success(domainCount),Failure(_) ,Failure(_)) => property -> ( domainCount.toDouble / globalCount, domainCount,0d, 0)
//          case (property, Failure(_), Success(domainCount),Failure(_) ,Success(rangeCount)) => property -> ( domainCount.toDouble / globalCount, domainCount, rangeCount.toDouble / globalCount, rangeCount)
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
      foundDomainCount = QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, true, property)
      foundRangeCount = QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, false, property)
      thresholdForStoring : Double= min(globalCount * requirementForStoringPropertyInDistribution, 100d)
      domainCount : Int = if(usedInDomain && foundDomainCount > thresholdForStoring) foundDomainCount else 0
      rangeCount : Int = if(usedInRange && foundRangeCount > thresholdForStoring) foundRangeCount else 0
      if (domainCount + rangeCount)  > thresholdForStoring
      importanceRatioDomain = if(usedInDomain && domainCount > thresholdForStoring) domainCount.toDouble / globalCount else 0d
      importanceRatioRange = if(usedInRange && rangeCount > thresholdForStoring) rangeCount.toDouble / globalCount else 0d
    }yield property -> (importanceRatioDomain, domainCount, importanceRatioRange, rangeCount)}.toMap
    if(globalCount > thresholdForStoringPropertyDistributionsLocally) {
    UpdateQueryFactory.addPropertyDistribution(typeEntity, distribution)}
    return distribution
  }

  val overlapMinimum = 0.5d

  def propertyDistributionOverlap(distributionEntityType: Map[String, (Double, Int, Double, Int)], comparableEntity: String)(implicit knowledgeGraph: KnowledgeGraph) : Double = {
    val comparableEntityDistribution = propertyDistributionIgnoreRareness(comparableEntity)
    return propertyDistributionOverlap(distributionEntityType, comparableEntityDistribution)
  }

  def findComparableTypes(entityType: String)(implicit knowledgeGraph: KnowledgeGraph) : Iterable[String] = {
    val distributionEntityType = propertyDistributionIgnoreRareness(entityType)
    val domainProperties = distributionEntityType.collect{
      case (property, (_, domainCount, _, _)) if domainCount > 0=> property
    }
    val rangeProperties = distributionEntityType.collect{
      case (property, (_, _,_, rangeCount)) if rangeCount > 0 => property
    }
    val comparableTypesRaw = QueryFactoryJena.comparableTypesPropertyDistribution(domainProperties, rangeProperties)
    val maximumCount = domainProperties.size + rangeProperties.size
    val minimumCount = Math.round(overlapMinimum * maximumCount)
    return comparableTypesRaw.takeWhile(_._2 >= minimumCount)
    .filter(comparableEntity => propertyDistributionOverlap(distributionEntityType, comparableEntity._1) > overlapMinimum)
    .map(_._1)
  }

}
