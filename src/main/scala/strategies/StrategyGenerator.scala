package strategies

import query.specific.QueryFactory
import rdf.GraphRDF

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 09.11.2016.
  */
object StrategyGenerator {
  def generateStrategies(entityGraph: GraphRDF): Array[Strategy] = {
    val strategies = new ArrayBuffer[Strategy]()
    for (statement <- entityGraph.statements) {
      val (isSubject, property, value) = statement match {
        case (entityGraph.entity, p, o) => (true, p, o)
        case (s, p, entityGraph.entity) => (false, p, s)
//        case (s, p, o) => println(s"$p is a property we have no strategies for...."); (true, )
        case _ => throw new Exception("A statement from the entities graph did NOT have the entity as subject or object!!" + statement)
      }
      val strategyStrings = QueryFactory.getStrategies(property)
      for (s <- strategyStrings) {
        matchStrategyClassNameToStrategy(s, property, isSubject, value, entityGraph.entity) match {
          case Some(s) if(!strategies.contains(s)) => strategies.append(s)
          case None => Unit
          case _ => Unit
        }
      }
    }
    return strategies.toArray
  }

  def generateInBNotInAStrategies(otherEntities: List[String]): List[Strategy] = {
    return Nil
  }

  def matchStrategyClassNameToStrategy(strategy: String, property: String, isSubject: Boolean, value: String, entity : String): Option[Strategy] = {
    return strategy match {
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#PropertyMatchStrategy" => Some(PropMatchStrategy(property, isSubject))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy" => Some(AlternativeLinkStrategy(property, isSubject))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#DirectLinkStrategy" => Some(DirectLinkStrategy(property, isSubject, entity))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchStrategy" => Some(ValueMatchStrategy(property, isSubject, value))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#HierarchyMatchStrategy" => Some(HierarchyMatchStrategy(property, isSubject))
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#InANotInBStrategy" => Some(InANotInBStrategy(property, isSubject, value))
      case _ => None
    }
  }

}
