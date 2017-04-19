package core.strategies

import core.globals.{KnowledgeGraph, SimilarPropertyOntology}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.SimilarPropertyOntology.SimilarPropertyOntology
import core.query.specific.{AskQuery, QueryFactory, UpdateQueryFactory}
import iAndO.dump.DumpObject
import preprocessing.ownOntologyPopularizer.IsDescriptivePropertyClassifier
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
  private val filenamePropToIsDescriptive = s"$knowledgeGraph-prop-is-descriptive"
  var mapPropertyToStrategies = convertPropertiesAndStrategiesToMap
  var mapPropertyToDomainCounts = getDomainCounts
  var mapPropertyToRangeCounts = getRangeCounts
  var mapPropertyToIsDescriptive = getIsDescriptive

  private def getIsDescriptive: Map[String, Boolean] = {
    if(StrategyFactory.forceRead) {
      val (properties, isDescriptive) = QueryFactory.findIsDescriptive()
      val mapped = properties.zip(isDescriptive).toMap
      DumpObject.dumpMapStringBoolean(mapped, filenamePropToIsDescriptive)
      return mapped
    } else {
      DumpObject.getMapStringBoolean(filenamePropToIsDescriptive)
    }
  }
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

  def setupStrategyFactory(activeStrategies : Seq[String])(implicit knowledgeGraph: KnowledgeGraph): Unit = {
    forceRead = true
    val sFactory = getStrategyFactory(knowledgeGraph)
    sFactory.mapPropertyToStrategies = sFactory.mapPropertyToStrategies.map{
      case (key, strategies) => (key, strategies
        .filter(strategyUri => activeStrategies.contains(StrategyNameFactory.getNameFromStrategyURI(strategyUri))))
    }
    forceRead = false
  }

  def isDescriptive(prop: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    getStrategyFactory.mapPropertyToIsDescriptive.get(prop) match {
      case Some(isDescriptive) => isDescriptive
      case None => {
        """P\d+q$""".r.findFirstIn(prop) match {
          case Some(a) => {
            val correctPropertyName = s"${KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)}${a.init}"
            getStrategyFactory.mapPropertyToIsDescriptive.get(correctPropertyName) match {
              case Some(b) => b
              case None => false
            }
          }
          case None => false
        }
        false
      }

    }
  }
  def getStrategyFactory(implicit knowledgeGraph: KnowledgeGraph) : StrategyFactory = {
    knowledgeGraph match {
      case KnowledgeGraph.wikidata => {
        if (strategyFactoryWikidata == null) {
          strategyFactoryWikidata = new StrategyFactory()
        }
        return strategyFactoryWikidata
      }
      case KnowledgeGraph.dbPedia => {
        if (strategyFactoryDBpedia == null) {
          strategyFactoryDBpedia = new StrategyFactory()
        }
        return strategyFactoryDBpedia
      }
    }
  }

  def getStrategies(entity: String, rdfTypes: List[String], property: String, isSubject: Boolean, rangeOrDomain: List[String])(implicit knowledgeGraph: KnowledgeGraph): List[Strategy] = {
    def getDomain = {
      if (isSubject) Nil else rangeOrDomain
    }

    def getRange = {
      if (isSubject) rangeOrDomain else Nil
    }
    val sFactory = getStrategyFactory(knowledgeGraph)
    if (property == KnowledgeGraph.getTypeProperty(knowledgeGraph)) return Nil
    val foundStrategies = sFactory.mapPropertyToStrategies.get(property) match {
      case Some(strategyList) => strategyList.map(s => matchStrategyClassNameToStrategy(s, property, getDomain, getRange, entity, rdfTypes)(knowledgeGraph, sFactory))
        .filter(s => s.isDefined)
        .flatMap(option => option.getOrElse(throw new Exception("not defined")))
      case None => Nil
    }
      return foundStrategies
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
          strategies.append(PropertyMatchStrategy(property, true, rdfTypes, count))
        }
        if (domain.nonEmpty) {
          val count = strategyFactory.mapPropertyToRangeCounts(property)
          strategies.append(PropertyMatchStrategy(property, false, rdfTypes, count))
        }
        return Some(strategies)
      }
//      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy" if (MyConfiguration.alActive) => {
//        val strategies = ArrayBuffer[Strategy]()
//        val (filteredDomain, filteredRange) = getDomainAndRangeWithCorrectType(domain, range, rdfTypes)
//        if (filteredDomain.nonEmpty) {
//          val weight = strategyFactory.mapPropertyToDomainCounts(property)
//          strategies += AlternativeLinkStrategy(property, Set() ++ filteredDomain, true)
//        }
//        if (filteredRange.nonEmpty) {
//          val weight = strategyFactory.mapPropertyToRangeCounts(property)
//          strategies += AlternativeLinkStrategy(property, Set() ++ filteredRange, true)
//        }
//        if (strategies.isEmpty) return None
//        return Some(strategies)
//      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#DirectLinkStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
//        println(s"About to find direct links for $property with $domain domain and range = $range")
        val (filteredDomain, filteredRange) = if(MyConfiguration.useRdfType) getDomainAndRangeWithCorrectType(domain, range, rdfTypes) else (domain, range)
        if (filteredDomain.nonEmpty) {
//          println("Created direct link strategy..")
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
      case a if a==SimilarPropertyOntology.searchDirectedL1Strategy.toString => {
        if(range.nonEmpty) return Some(List(SearchDirectedL1Strategy(property, range)))
        else None
      }
      case a if a==SimilarPropertyOntology.searchDirectedL2Strategy.toString => {
        if(range.nonEmpty) return Some(List(SearchDirectedL2Strategy(property, range)))
        else None
      }
      case a if a==SimilarPropertyOntology.searchUndirectedL1Strategy.toString => {
        return Some(List(SearchUndirectedL1Strategy(property, range ++ domain)))
      }
      case a if a==SimilarPropertyOntology.searchUndirectedL2Strategy.toString => {
        return Some(List(SearchUndirectedL2Strategy(property, range ++ domain)))
      }
      case a if a==SimilarPropertyOntology.expandNodeStrategy.toString => {
        return Some(List(ExpandNodeStrategy(property, range ++ domain, rdfTypes)))
      }
      case a if a==SimilarPropertyOntology.aggregatorStrategy.toString => {
        val strategies = ArrayBuffer[Strategy]()
        if(domain.size > 1){
          strategies += AggregatorStrategy(entity, property, false, domain, rdfTypes)
        }
        if(range.size > 1){
          strategies += AggregatorStrategy(entity, property, true, range, rdfTypes)
        }
        return Some(strategies.toList)
      }
      case _ => None
    }
  }

  def valueIsAPotentialValueMatchFindCount(value: String, property: String, valueIsSubject: Boolean)(implicit knowledgeGraph: KnowledgeGraph): Option[Int] = {
    if(!value.startsWith("http")) return None
    QueryFactory.getValueMatchFromExistingDb(value, property) match {
      case Some(s) => return Some(s)
      case None => {
        val countFromDs = if (!valueIsSubject) QueryFactory.findCountForPropertyWithValue(property, value) else
          QueryFactory.findCountForPropertyWithSubject(property, value)
        countFromDs match {
          case Success(count) => {
            UpdateQueryFactory.updateValueCount(property, value, count) //Stores the value so we don't have to do the full count again..
            return Some(count)
          }
          case Failure(m) => {
            println(s"Unable to find count for: $property with value: $value isSubject: $valueIsSubject\n Failure message: $m")
            return None
          }
        }
      }
    }
  }
}
