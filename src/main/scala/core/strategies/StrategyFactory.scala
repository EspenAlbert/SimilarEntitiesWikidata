package core.strategies

import core.globals.KnowledgeGraph
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.{AskQuery, QueryFactory, UpdateQueryFactory}
import iAndO.dump.DumpObject
import similarityFinder.MyConfiguration

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success}

/**
  * Created by espen on 09.03.17.
  */
class StrategyFactory(implicit knowledgeGraph: KnowledgeGraph) {
  private val filenamePropToStrategies = s"$knowledgeGraph-prop-strategies"
  private val filenamePropToDomainCount = s"$knowledgeGraph-prop-domain-count"
  private val filenamePropToRangeCount = s"$knowledgeGraph-prop-range-count"
  val mapPropertyToStrategies = convertPropertiesAndStrategiesToMap
  val mapPropertyToDomainCounts = getDomainCounts
  val mapPropertyToRangeCounts = getRangeCounts

  private def getDomainCounts: Map[String, Int] = {
    if(StrategyFactory.forceRead) {
      val (properties, strategies) = QueryFactory.findAllDomainCounts()
      val mapped = properties.zip(strategies).toMap
      DumpObject.dumpMapStringInt(mapped, filenamePropToDomainCount)
      return mapped
    } else {
      DumpObject.getMapStringInt(filenamePropToDomainCount)
    }
  }
  private def getRangeCounts: Map[String, Int] = {
    if(StrategyFactory.forceRead) {
      val (properties, strategies) = QueryFactory.findAllRangeCounts()
      val mapped = properties.zip(strategies).toMap
      DumpObject.dumpMapStringInt(mapped, filenamePropToRangeCount)
      return mapped
    } else {
      DumpObject.getMapStringInt(filenamePropToRangeCount)
    }
  }


  private def convertPropertiesAndStrategiesToMap: Map[String, List[String]] = {
    if(StrategyFactory.forceRead) {
      val (properties, strategies) = QueryFactory.findAllStrategies()
      val mapped = properties.zip(strategies)
        .groupBy(pAndS => pAndS._1)
        .map(propAndTuple => propAndTuple._1 -> propAndTuple._2.map(_._2))
      DumpObject.dumpJsonMapStringListString(mapped, filenamePropToStrategies)
      return mapped
    } else {
      DumpObject.getStringMap(filenamePropToStrategies)
    }
  }
}
object StrategyFactory {
  var forceRead = false
  var strategyFactoryWikidata: StrategyFactory = null
  var strategyFactoryDBpedia: StrategyFactory = null

  def getStrategies(entity: String, rdfTypes: List[String], property: String, isSubject: Boolean, rangeOrDomain: List[String])(implicit knowledgeGraph: KnowledgeGraph): List[Strategy] = {

    def getDomain = {
      if (isSubject) Nil else rangeOrDomain
    }

    def getRange = {
      if (isSubject) rangeOrDomain else Nil
    }

    knowledgeGraph match {
      case KnowledgeGraph.wikidata => {
        if (strategyFactoryWikidata == null) {
          strategyFactoryWikidata = new StrategyFactory()
        }
        return strategyFactoryWikidata.mapPropertyToStrategies.get(property) match {
          case Some(strategyList) => strategyList.map(s => matchStrategyClassNameToStrategy(s, property, getDomain, getRange, entity, rdfTypes)(knowledgeGraph, strategyFactoryWikidata))
            .filter(s => s.isDefined)
            .flatMap(option => option.getOrElse(throw new Exception("not defined")))
        }
      }
    }
  }

  def getDomainAndRangeWithCorrectType(domain: List[String], range: List[String], rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph): (List[String], List[String]) = {
    val filteredRange = range.filter((s) => AskQuery.subjectHasType(s, rdfTypes))
    val filteredDomain = domain.filter((s) => AskQuery.subjectHasType(s, rdfTypes))
    return (filteredDomain, filteredRange)
  }

  //    def matchStrategyClassNameToStrategy(strategy: String, property: String, domain: List[String], range: List[String], entity: String, rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph):
  def matchStrategyClassNameToStrategy(strategy: String, property: String, domain: List[String], range: List[String], entity: String, rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph, strategyFactory : StrategyFactory): Option[Seq[Strategy]] = {
    return strategy match {
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#PropertyMatchStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (range.nonEmpty) {
          val count =strategyFactory.mapPropertyToDomainCounts(property)
          strategies.append(PropMatchStrategy(property, true, rdfTypes, count))
        }
        if (domain.nonEmpty) {
          val count = strategyFactory.mapPropertyToRangeCounts(property)
          strategies.append(PropMatchStrategy(property, false, rdfTypes, count))
        }
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy" if (MyConfiguration.alActive) => {
        val strategies = ArrayBuffer[Strategy]()
        val (filteredDomain, filteredRange) = getDomainAndRangeWithCorrectType(domain, range, rdfTypes)
        if (filteredDomain.nonEmpty) {
          val weight = strategyFactory.mapPropertyToDomainCounts(property)
          strategies += AlternativeLinkStrategy(property, Set() ++ filteredDomain, true)
        }
        if (filteredRange.nonEmpty) {
          val weight = strategyFactory.mapPropertyToRangeCounts(property)
          strategies += AlternativeLinkStrategy(property, Set() ++ filteredRange, true)
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#DirectLinkStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        val (filteredDomain, filteredRange) = getDomainAndRangeWithCorrectType(domain, range, rdfTypes)
        if (filteredDomain.nonEmpty) {
          println("Created direct link strategy..")
          strategies += DirectLinkStrategy(property, Set() ++ filteredDomain)
        }
        if (filteredRange.nonEmpty) {
          strategies += DirectLinkStrategy(property, Set() ++ filteredRange)
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchSubjectStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (domain.nonEmpty) {
          for (d <- domain) {
            valueIsAPotentialValueMatchFindCount(d, property, false) match {
              case Some(count) => strategies += ValueMatchStrategy(property, false, d, rdfTypes, count)
              case None => Unit
            }
          }
          //          if(MyConfiguration.inANotInBActive) {
          //            val domainCount = strategyFactory.mapPropertyToDomainCounts(property)
          //            val domainPotentialValueMatches = strategies.map((s) => if (s.isInstanceOf[ValueMatchStrategy]) s.asInstanceOf[ValueMatchStrategy].value)
          //            if (domainPotentialValueMatches.length > 0) {
          //              val weight = getPropMatchWeight(SimilarPropertyOntology.maxCountForProperties - domainCount)
          //              strategies += InANotInBStrategy(property, true, domainPotentialValueMatches.asInstanceOf[Iterable[String]], (1.toDouble / domainPotentialValueMatches.length) * MyConfiguration.inANotInBBoost * weight)
          //              //As long as inANotInBBoost < 1, then it will never be more negative than a PropertyMatchStrategy...
          //            }
          //          }
          //          core.strategies += InANotInBStrategy(property, false, domain, domain.length * MyConfiguration.inANotInBBoost * logarithmicWeightForCount(domainCount))
          //          core.strategies += InBNotInAStrategy(property, false, domain, domain.length * MyConfiguration.inBNotInABoost * logarithmicWeightForCount(domainCount))
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchObjectStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (range.nonEmpty) {
          for (r <- range) {
            valueIsAPotentialValueMatchFindCount(r, property, true) match {
              case Some(count) => strategies += ValueMatchStrategy(property, true, r, rdfTypes, count)
              case None => Unit
            }
          }
          //          if(MyConfiguration.inANotInBActive) {
          //            val rangeCount = strategyFactory.mapPropertyToRangeCounts(property)
          //            val rangePotentialValueMatches = strategies.map((s) => if(s.isInstanceOf[ValueMatchStrategy]) s.asInstanceOf[ValueMatchStrategy].value)
          //            if(rangePotentialValueMatches.length > 0) {
          //              val weight = getPropMatchWeight(SimilarPropertyOntology.maxCountForProperties - rangeCount)
          //              strategies += InANotInBStrategy(property, true, rangePotentialValueMatches.asInstanceOf[Iterable[String]], (1.toDouble / rangePotentialValueMatches.length) * MyConfiguration.inANotInBBoost * weight)
          //              //As long as inANotInBBoost < 1, then it will never be more negative than a PropertyMatchStrategy...
          //            }
          //          }

          //          core.strategies += InBNotInAStrategy(property, true, range, range.length * MyConfiguration.inBNotInABoost * logarithmicWeightForCount(rangeCount))
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#HierarchyMatchStrategy" =>
        None //Some(HierarchyMatchStrategy(property, isSubject))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#InANotInBStrategy" =>
        None
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#TimeProperty" => {
        try {
          assert(range.length == 1)
        } catch {
          case a: Throwable => println("more than 1 date time value", range)
        }
        // Only one value
        val dbCount = strategyFactory.mapPropertyToDomainCounts(property)
        return Some(List(DateComparisonStrategy(property, range(0), dbCount)))
      }
      case _ => None
    }
  }

  def valueIsAPotentialValueMatchFindCount(value: String, property: String, isSubject: Boolean)(implicit knowledgeGraph: KnowledgeGraph): Option[Int] = {
    QueryFactory.getValueMatchFromExistingDb(value, property) match {
      case Some(s) => return Some(s)
      case None => {
        val countFromDs = if (isSubject) QueryFactory.findCountForPropertyWithValue(property, value) else
          QueryFactory.findCountForPropertyWithSubject(property, value)
        countFromDs match {
          case Success(count) => {
            UpdateQueryFactory.updateValueCount(property, value, count) //Stores the value so we don't have to do the full count again..
            return Some(count)
          }
          case Failure(m) => {
            println(s"Unable to find count for: $property with value: $value isSubject: $isSubject\n Failure message: $m")
            return None
          }
        }
      }
    }
  }
}
