package core.rdf

import breeze.linalg.{max, min}
import breeze.numerics.{abs, ceil}
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.SimilarPropertyOntology
import core.query.specific.{AskQuery, DatasetInferrer, QueryFactoryJena, UpdateQueryFactory}

import scala.collection.mutable
import scala.util.{Failure, Success}

/**
  * Created by espen on 04.05.17.
  */
object TypePropertyDistributionFinder {

  val thresholdForStoringPropertyDistributionsLocally = 100
  val requirementForStoringPropertyInDistribution = 0.0001
  val alwaysStoreCountWhenAbove = 100d
  val neverStoreCountWhenBelow = 10

  def propertyDistributionOverlapUseMinimum(distributionA :Map[String, (Double, Int, Double, Int)], distributionB: Map[String, (Double, Int, Double,Int)]): Double = {
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

  def propertyDistributionIgnoreRareness(typeEntity: String)(implicit knowledgeGraph: KnowledgeGraph) : Map[String, (Double, Int, Double, Int)] = {
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
    return createPropertyDistributionForTypeFindAllPropertiesAtOnce(typeEntity, globalCount)
  }


  def createPropertyDistributionForType(typeEntity: String, globalCount: Int)(implicit knowledgeGraph: KnowledgeGraph): Map[String, (Double, Int, Double, Int)] = {
    val domainProps = QueryFactoryJena.propertiesWhereDomainHasType(typeEntity).map((_, true))
    val rangeProps = QueryFactoryJena.propertiesWhereRangeHasType(typeEntity).map((_, false))
    val propertiesAndIsSubject: List[(String, Boolean)] = domainProps ++ rangeProps
    val distribution = {
      for {
        (property: String, (usedInDomain, usedInRange)) <- propertiesAndIsSubject.groupBy(_._1).map {
          case (property, (list)) => ((property), (list.exists(_._2), list.exists(!_._2)))
        }
        foundDomainCount = QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, true, property)
        foundRangeCount = QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, false, property)
        thresholdForStoring: Double = min(globalCount * requirementForStoringPropertyInDistribution, alwaysStoreCountWhenAbove)
        domainCount: Int = if (usedInDomain && foundDomainCount > thresholdForStoring) foundDomainCount else 0
        rangeCount: Int = if (usedInRange && foundRangeCount > thresholdForStoring) foundRangeCount else 0
        if (domainCount + rangeCount) > thresholdForStoring
        importanceRatioDomain = if (usedInDomain && domainCount > thresholdForStoring) domainCount.toDouble / globalCount else 0d
        importanceRatioRange = if (usedInRange && rangeCount > thresholdForStoring) rangeCount.toDouble / globalCount else 0d
      } yield property -> (importanceRatioDomain, domainCount, importanceRatioRange, rangeCount)
    }.toMap
    return distribution
  }
  def createPropertyDistributionForTypeUsingProperties(typeEntity: String, globalCount: Int, propertiesWithDomainCounts : List[(String, Int)])(implicit knowledgeGraph: KnowledgeGraph): Map[String, (Double, Int, Double, Int)] = {
    val thresholdForStoring: Double = min(globalCount * requirementForStoringPropertyInDistribution, alwaysStoreCountWhenAbove)
    val distribution = {
      for {
        (property, domainCountTotalForProperty) <-propertiesWithDomainCounts
        if domainCountTotalForProperty > thresholdForStoring
        foundDomainCount = QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, true, property)
        foundRangeCount = QueryFactoryJena.countEntitiesOfTypeForProperty(typeEntity, false, property)
        domainCount: Int = if (foundDomainCount > thresholdForStoring) foundDomainCount else 0
        rangeCount: Int = if (foundRangeCount > thresholdForStoring) foundRangeCount else 0
        if (domainCount + rangeCount) > thresholdForStoring
        importanceRatioDomain = if (domainCount > thresholdForStoring) domainCount.toDouble / globalCount else 0d
        importanceRatioRange = if (rangeCount > thresholdForStoring) rangeCount.toDouble / globalCount else 0d
      } yield property -> (importanceRatioDomain, domainCount, importanceRatioRange, rangeCount)
    }.toMap
    return distribution
  }
  def createPropertyDistributionForTypeFindAllPropertiesAtOnce(typeEntity: String, globalCount: Int)(implicit knowledgeGraph: KnowledgeGraph): Map[String, (Double, Int, Double, Int)] = {
    val thresholdForStoring = max(ceil(min(globalCount * requirementForStoringPropertyInDistribution, alwaysStoreCountWhenAbove)).toInt, neverStoreCountWhenBelow)
    val domainPropertiesAndCounts = QueryFactoryJena.propertiesAndCountsForType(typeEntity, true, thresholdForStoring).map(pair => (pair, true))
    val rangePropertiesAndCounts = QueryFactoryJena.propertiesAndCountsForType(typeEntity, false, thresholdForStoring).map(pair => (pair, false))
    val propertiesAndCounts = (domainPropertiesAndCounts ++ rangePropertiesAndCounts).foldLeft(mutable.HashMap[String, (Int, Int)]().withDefaultValue((0, 0))) {
      case (acc, ((property, count), isSubject)) => {
        if (isSubject) {
          val rangeCount = acc(property)._2
          acc.update(property, (count, rangeCount))
        }
        else {
          val domainCount = acc(property)._1
          acc.update(property, (domainCount, count))
        }
        acc
      }
    }
    (for {
      (property, (domainCount, rangeCount)) <- propertiesAndCounts
      importanceRatioDomain = if (domainCount > thresholdForStoring) domainCount.toDouble / globalCount else 0d
      importanceRatioRange = if (rangeCount > thresholdForStoring) rangeCount.toDouble / globalCount else 0d
    } yield property -> (importanceRatioDomain, domainCount, importanceRatioRange, rangeCount)).toMap
  }


  val overlapMinimum = 0.5d

  def propertyDistributionOverlap(distributionEntityType: Map[String, (Double, Int, Double, Int)], comparableEntity: String, combineFunction: (Double, Double) => Double = min(_:Double,_:Double))(implicit knowledgeGraph: KnowledgeGraph) : Double = {
    val comparableEntityDistribution = propertyDistributionIgnoreRareness(comparableEntity)
    return propertyDistributionOverlapChooseCombineFunction(distributionEntityType, comparableEntityDistribution, combineFunction)
  }
  def propertyDistributionOverlapChooseCombineFunction(distributionA :Map[String, (Double, Int, Double, Int)], distributionB: Map[String, (Double, Int, Double,Int)],combineFunction: (Double, Double) => Double): Double = {
    val abOverlap: Double = calculateOverlap(distributionA, distributionB)
    val baOverlap: Double = calculateOverlap(distributionB, distributionA)
    return combineFunction(abOverlap, baOverlap)

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
