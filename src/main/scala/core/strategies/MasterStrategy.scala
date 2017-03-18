package core.strategies
import breeze.numerics._
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.SimilarPropertyOntology
import core.globals.SimilarPropertyOntology._
import core.query.specific.{AskQuery, QueryFactory, UpdateQueryFactory}
import similarityFinder.MyConfiguration

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 15.11.2016.
  */
class MasterStrategy (statements : List[Tuple3[String, String, String]], entity : String, typeString : List[String])(implicit knowledgeGraph: KnowledgeGraph){

  val property = statements(0)._2
  private val domain = ArrayBuffer[String]()
  private val range = ArrayBuffer[String]()
  for (statement <- statements) {
    val (isSubject, property, value) = statement match {
      case (`entity`, p, o) => (true, p, o)
      case (s, p, `entity`) => (false, p, s)
      case _ => throw new Exception("A statement from the entities graph did NOT have the entity as subject or object!!" + statement)
    }
    if(isSubject) range.append(value) else domain.append(value)
  }
  private val strategies = ArrayBuffer[Strategy]()
  val strategyStrings = StrategyFactory.getStrategies(property)
    for (s <- strategyStrings) {
      MasterStrategy.matchStrategyClassNameToStrategy(s, property, domain.toList, range.toList, entity, typeString) match {
        case Some(s) => strategies.append(s :_*);
        case _ => Unit
      }

    }

  def getCompositeStrategies() : List[Strategy] = {
    if(strategies.exists(_.isInstanceOf[DirectLinkStrategy])) return strategies.filterNot(_.isInstanceOf[ValueMatchStrategy]).toList
    return strategies.toList
  }
}
//This is where all core.strategies are created
object MasterStrategy {
  def logarithmicWeightForCount(countForProperty: Int): Double = {
    return log(maxCountForProperties / countForProperty)
  }



  def getDomainAndRangeWithCorrectType(domain: List[String], range: List[String], rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph): (List[String], List[String]) = {
    val filteredRange = range.filter((s) => AskQuery.subjectHasType(s, rdfTypes))
    val filteredDomain = domain.filter((s) => AskQuery.subjectHasType(s, rdfTypes))
    return (filteredDomain, filteredRange)
  }
  def matchStrategyClassNameToStrategy(strategy: String, property: String, domain: List[String], range: List[String], entity: String, rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph): Option[Seq[Strategy]] = {
    return strategy match {
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#PropertyMatchStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (range.nonEmpty) {
          val count = QueryFactory.findDomainCount(property)
          val weight = getPropMatchWeight(count)
          strategies.append(PropMatchStrategy(property, true, weight, rdfTypes, count < MyConfiguration.maxCountForValueMatchesToFindSimlars))
        }
        if (domain.nonEmpty) {
          val count = QueryFactory.findRangeCount(property)
          val weight = getPropMatchWeight(count)
          strategies.append(PropMatchStrategy(property, false, weight, rdfTypes, count < MyConfiguration.maxCountForValueMatchesToFindSimlars))

        }
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy" if(MyConfiguration.alActive) => {
        val strategies = ArrayBuffer[Strategy]()
        val (filteredDomain, filteredRange) = getDomainAndRangeWithCorrectType(domain, range, rdfTypes)
        if (filteredDomain.nonEmpty) {
          val weight = QueryFactory.findDomainCount(property)
          strategies += AlternativeLinkStrategy(property, Set() ++ filteredDomain, true, MyConfiguration.alternativeLinkNegative * logarithmicWeightForCount(weight))
        }
        if (filteredRange.nonEmpty) {
          val weight = QueryFactory.findRangeCount(property)
          strategies += AlternativeLinkStrategy(property, Set() ++ filteredRange, true, MyConfiguration.alternativeLinkNegative * logarithmicWeightForCount(weight))
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#DirectLinkStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        val (filteredDomain, filteredRange) = getDomainAndRangeWithCorrectType(domain, range, rdfTypes)
        if (filteredDomain.nonEmpty) {
          println("Created direct link strategy..")
          strategies += DirectLinkStrategy(property, Set() ++ filteredDomain, MyConfiguration.directLinkBoost * logarithmicWeightForCount(filteredDomain.length))
        }
        if (filteredRange.nonEmpty) {
          strategies += DirectLinkStrategy(property, Set() ++ filteredRange, MyConfiguration.directLinkBoost * logarithmicWeightForCount(filteredRange.length))
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
        None //Some(DirectLinkStrategy(property, isSubject, entity))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchSubjectStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (domain.nonEmpty) {
          for (d <- domain) {
            valueIsAPotentialValueMatchFindCount(d, property, false) match {
              case Some(count) => strategies += ValueMatchStrategy(property, false, d, rdfTypes, MyConfiguration.valueMatchBoost * logarithmicWeightForCount(count), count < MyConfiguration.maxCountForValueMatchesToFindSimlars)
              case None => Unit
            }
          }
          if(MyConfiguration.inANotInBActive) {
            val domainCount = QueryFactory.findDomainCount(property)
            val domainPotentialValueMatches = strategies.map((s) => if (s.isInstanceOf[ValueMatchStrategy]) s.asInstanceOf[ValueMatchStrategy].value)
            if (domainPotentialValueMatches.length > 0) {
              val weight = getPropMatchWeight(SimilarPropertyOntology.maxCountForProperties - domainCount)
              strategies += InANotInBStrategy(property, true, domainPotentialValueMatches.asInstanceOf[Iterable[String]], (1.toDouble / domainPotentialValueMatches.length) * MyConfiguration.inANotInBBoost * weight)
              //As long as inANotInBBoost < 1, then it will never be more negative than a PropertyMatchStrategy...
            }
          }
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
              case Some(count) => strategies += ValueMatchStrategy(property, true, r, rdfTypes, MyConfiguration.valueMatchBoost * logarithmicWeightForCount(count), count < MyConfiguration.maxCountForValueMatchesToFindSimlars)
              case None => Unit
            }
          }
          if(MyConfiguration.inANotInBActive) {
            val rangeCount = QueryFactory.findRangeCount(property)
            val rangePotentialValueMatches = strategies.map((s) => if(s.isInstanceOf[ValueMatchStrategy]) s.asInstanceOf[ValueMatchStrategy].value)
            if(rangePotentialValueMatches.length > 0) {
              val weight = getPropMatchWeight(SimilarPropertyOntology.maxCountForProperties - rangeCount)
              strategies += InANotInBStrategy(property, true, rangePotentialValueMatches.asInstanceOf[Iterable[String]], (1.toDouble / rangePotentialValueMatches.length) * MyConfiguration.inANotInBBoost * weight)
              //As long as inANotInBBoost < 1, then it will never be more negative than a PropertyMatchStrategy...
            }
          }

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
          case a : Throwable => println("more than 1 date time value", range)
        }
          // Only one value
          val weight = logarithmicWeightForCount(QueryFactory.findDomainCount(property)) * MyConfiguration.dateComparisonWeight
          return Some(List(DateComparisonStrategy(property, range(0), weight)))
        }
      case _ => None
    }
  }

  private def getPropMatchWeight(count: Int): Double = {
    return if(logarithmicWeightForCount(count) > MyConfiguration.maximumWeightPropertyMatch) MyConfiguration.maximumWeightPropertyMatch else logarithmicWeightForCount(count)
  }

  def valueIsAPotentialValueMatchFindCount(value: String, property: String, isSubject: Boolean)(implicit knowledgeGraph: KnowledgeGraph): Option[Int] = {
    QueryFactory.getValueMatchFromExistingDb(value, property) match {
      case Some(s) => return Some(s)
      case None => {
        val countFromDs = if (isSubject) QueryFactory.findDistinctCountForPropertyWithValue(property, value) else
          QueryFactory.findDistinctCountForPropertyWithSubject(property, value)
        countFromDs match {
          case Some(count) => {
            UpdateQueryFactory.updateValueCount(property, value, count) //Stores the value so we don't have to do the full count again..
            return Some(count)
          }
          case None => {
            println(s"Unable to find count for: $property with value: $value isSubject: $isSubject")
            return None
          }
        }
      }
    }
  }
}
