package core.rdf

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.SimilarPropertyOntology
import core.query.specific.QueryFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future, TimeoutException, future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by Espen on 09.11.2016.
  */
class GraphRDF(val entity : String)(implicit val knowledgeGraph: KnowledgeGraph) {

  lazy val entityIsObjectStatements = Future{
    QueryFactory.findSubjectsAndProperties(entity)
  }
  val entityIsSubjectStatments = Future{
    QueryFactory.findPropertiesAndObjects(entity)
  }
  lazy val statementsList = getStatementsList
  private def getStatementsList: List[(String,String,String)]= {
    var eIsObject : (List[String], List[String]) = (Nil, Nil)
    var eIsSubject : (List[String], List[String]) = (Nil, Nil)
    try{
      eIsObject = Await.result(entityIsObjectStatements, 100 seconds)
      eIsSubject = Await.result(entityIsSubjectStatments, 100 seconds)
      assert(eIsObject._1.size > 0, s"There are no statements where $entity is object")
      assert(eIsSubject._1.size > 0, s"There are no statements where $entity is subject")
    } catch {
      case a: AssertionError => println(s"Result might not be as expected for entity: $entity most likely have missing statements...")
      case a : TimeoutException => println(s"Unable to find all statements within 100 s for $entity")
    }
    val statements = (for ((subject, property) <- eIsObject._1 zip eIsObject._2) yield (subject, property, entity))
      .++(for ((property, objectValue) <- eIsSubject._2 zip eIsSubject._1) yield (entity, property, objectValue))
    return statements
  }
  def isType(s: (String, String, String)): Boolean = {
    s match {
      case (s, "http://www.wikidata.org/entity/P31", o) => true
      case (s, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", o) => true
      case _ => false
    }
  }

  lazy val getTypes : List[String] = {
    statementsList.filter((s) => isType(s)).map(_._3)
  }
  def getStatementCountWithoutTypeStatements : Int = {
    return statementsList.size - getTypes.size
  }
  def findScalingFactor(graph: GraphRDF) : Double = {
    val otherEntity = graph.entity
    var overlaps = 0
    for(statement <- graph.statementsList) {
      statement match {
        case (`otherEntity`, p, value) => if(getProperties(s=true).exists(_ == p)) overlaps += 1
        case (value, p, `otherEntity`) => if(getProperties(o=true).exists(_ == p)) overlaps += 1
      }
    }
    return overlaps.toDouble /  graph.statementsList.size
  }
  def getProperties(s : Boolean = false, o : Boolean = false) : List[String] = {
    val properties = ArrayBuffer[String]()
    statementsList.foreach((f) => if(!s && !o || s && f._1 == entity || o && f._3 == entity) properties.append(f._2))
    return properties.toList
  }

  val tooCommonProperties = Set("P31")

  def getUniqueWikidataPropertiesWithoutTheMostCommon() : Set[String] = {
    return Set() ++ statementsList.filterNot((s) => (s._2.indexOf("P") > 0) && tooCommonProperties.contains(s._2.substring(s._2.indexOf("P")))).
      map((s) => s._2)
  }

}
