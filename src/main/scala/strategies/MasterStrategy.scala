package strategies
import breeze.numerics._
import globals.{MyConfiguration, SimilarPropertyOntology}
import globals.SimilarPropertyOntology._
import query.specific.{QueryFactory, QueryFactoryV2, UpdateQueryFactory}
import query.variables.OptionsForResultQueryVariable
import rdf.SimpleRDFFactory

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 15.11.2016.
  */
class MasterStrategy (statements : List[Tuple3[String, String, String]], entity : String, typeString : String){

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
  val strategyStrings = QueryFactory.getStrategiesUpdated(property)
    for (s <- strategyStrings) {
      MasterStrategy.matchStrategyClassNameToStrategy(s, property, domain.toList, range.toList, entity, typeString) match {
        case Some(s) => strategies.append(s :_*); val a = 5;
        case _ => Unit
      }

    }

  def getCompositeStrategies() : List[Strategy] = {
    return strategies.toList
  }
}
//This is where all strategies are created
object MasterStrategy {
  def logarithmicWeight(countForProperty: Int): Double = {
    return log(maxCountForProperties.toString.toInt / countForProperty)
  }


  def matchStrategyClassNameToStrategy(strategy: String, property: String, domain: List[String], range: List[String], entity: String, rdfType: String): Option[Seq[Strategy]] = {
    return strategy match {
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#PropertyMatchStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (range.nonEmpty) {
          val weight = QueryFactory.findDomainCount(property)
          strategies.append(PropMatchStrategy(property, true, logarithmicWeight(weight), rdfType))
        }
        if (domain.nonEmpty) {
          val weight = QueryFactory.findRangeCount(property)
          strategies.append(PropMatchStrategy(property, false, logarithmicWeight(weight), rdfType))

        }
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        val (filteredDomain, filteredRange) = getDomainAndRangeWithCorrectType(domain, range, rdfType)
        if (filteredDomain.nonEmpty) {
          val weight = QueryFactory.findDomainCount(property)
          strategies += AlternativeLinkStrategy(property, Set() ++ filteredDomain, true, MyConfiguration.alternativeLinkNegative * logarithmicWeight(weight))
        }
        if (filteredRange.nonEmpty) {
          val weight = QueryFactory.findRangeCount(property)
          strategies += AlternativeLinkStrategy(property, Set() ++ filteredRange, true, MyConfiguration.alternativeLinkNegative * logarithmicWeight(weight))
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#DirectLinkStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        val (filteredDomain, filteredRange) = getDomainAndRangeWithCorrectType(domain, range, rdfType)
        if (filteredDomain.nonEmpty) {
          strategies += DirectLinkStrategy(property, Set() ++ filteredDomain, MyConfiguration.directLinkBoost * logarithmicWeight(filteredDomain.length))
        }
        if (filteredRange.nonEmpty) {
          strategies += DirectLinkStrategy(property, Set() ++ filteredRange, MyConfiguration.directLinkBoost * logarithmicWeight(filteredRange.length))
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
        None //Some(DirectLinkStrategy(property, isSubject, entity))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchSubjectStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (domain.nonEmpty) {
          for (d <- domain) {
            valueIsAPotentialValueMatch(d, property, false) match {
              case Some(count) => strategies += ValueMatchStrategy(property, false, d, rdfType, MyConfiguration.valueMatchBoost * logarithmicWeight(count))
              case None => Unit
            }
          }
        }
        if (strategies.isEmpty) return None
        return Some(strategies)
      }
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchObjectStrategy" => {
        val strategies = ArrayBuffer[Strategy]()
        if (range.nonEmpty) {
          for (r <- range) {
            valueIsAPotentialValueMatch(r, property, true) match {
              case Some(count) => strategies += ValueMatchStrategy(property, true, r, rdfType, MyConfiguration.valueMatchBoost * logarithmicWeight(count))
              case None => Unit
            }
          }
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
          val weight = logarithmicWeight(QueryFactory.findDomainCount(property)) * MyConfiguration.dateComparisonWeight
          return Some(List(DateComparisonStrategy(property, range(0), weight)))
        }
      case _ => None
    }
  }

  def valueIsAPotentialValueMatch(value: String, property: String, isSubject: Boolean): Option[Int] = {
    try {
      //"?s " + OptionsForResultQueryVariable.distinct + " " + OptionsForResultQueryVariable.count
      getValueMatchFromExistingDb(value, property) match {
        case Some(s) => return Some(s)
        case None => Unit
      }
      val statement1 = if (isSubject) SimpleRDFFactory.getStatement("?s " + OptionsForResultQueryVariable.distinct + " " + OptionsForResultQueryVariable.count, property, value) else
        SimpleRDFFactory.getStatement(value, property, "?o " + OptionsForResultQueryVariable.distinct + " " + OptionsForResultQueryVariable.count)
      val count = QueryFactoryV2.findSingleValue(statement1)
      if (count > 1) {
        UpdateQueryFactory.updateValueCount(property, value, count) //Stores the value so we don't have to do the full count again..
        return Some(count)
      }
      return None
    } catch {
      case a: Throwable => println(a); return None
    }
  }

  def getValueMatchFromExistingDb(value: String, property: String): Option[Int] = {
    try {
      val statement1 = SimpleRDFFactory.getStatement(property, SimilarPropertyOntology.valueMatchProperty, "?o " + OptionsForResultQueryVariable.ignoreMe)
      val statement2 = SimpleRDFFactory.getStatement("?o " + OptionsForResultQueryVariable.ignoreMe, SimilarPropertyOntology.valueMatchValue, value)
      val statement3 = SimpleRDFFactory.getStatement("?o " + OptionsForResultQueryVariable.ignoreMe, SimilarPropertyOntology.valueMatchCount, "?c")
      val count = QueryFactoryV2.findSingleValue(statement1, statement2, statement3)
      return Some(count)
    } catch {
      case a: Throwable => return None
    }
  }

  def getDomainAndRangeWithCorrectType(domain: List[String], range: List[String], rdfType: String): (List[String], List[String]) = {
    val filteredRange = range.filter((s) => QueryFactory.ask(SimpleRDFFactory.getStatement((s, "w:P31", rdfType))))
    val filteredDomain = domain.filter((s) => QueryFactory.ask(SimpleRDFFactory.getStatement((s, "w:P31", rdfType))))
    return (filteredDomain, filteredRange)
  }

  def isSharableDomain(property: String): Boolean = {
    return QueryFactory.ask(SimpleRDFFactory.getStatement(property, SimilarPropertyOntology.sharableDomain, "?o"))
  }

  def isSharableRange(property: String): Boolean = {
    return QueryFactory.ask(SimpleRDFFactory.getStatement(property, SimilarPropertyOntology.sharableRange, "?o"))
  }
}
