package core.rdf

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.SimilarPropertyOntology
import core.query.specific.QueryFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future, future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by Espen on 09.11.2016.
  */
class GraphRDF(val entity : String)(implicit val knowledgeGraph: KnowledgeGraph) {

  val entityIsObjectStatements = Future{
    QueryFactory.findSubjectsAndProperties(entity)
  }
  val entityIsSubjectStatments = Future{
    QueryFactory.findPropertiesAndObjects(entity)
  }
  val statements = mutable.Set[Tuple3[String, String, String]]()
  def statementsList: List[(String,String,String)]= {
    val eIsObject = Await.result(entityIsObjectStatements, 10 seconds)
    val eIsSubject = Await.result(entityIsSubjectStatments, 10 seconds)
    assert(eIsObject._1.size > 0, s"There are no statements where $entity is object")
    assert(eIsSubject._1.size > 0, s"There are no statements where $entity is subject")
    val statements = (for ((subject, property) <- eIsObject._1 zip eIsObject._2) yield (subject, property, entity))
      .++(for ((property, objectValue) <- eIsSubject._2 zip eIsSubject._1) yield (entity, property, objectValue))
    return statements.toList
  }
//  val preparedList = Future[List[(String, String, String)]](statementsList)
//  val b = preparedList.onComplete()
//  private val statementsList2 = statements.toList
//  def getStatementsList(): List[(String, String, String)] = {
//    Thread.sleep(1000)
//    return statementsList2
//  }
  val executionIsDone = true
  def isType(s: (String, String, String)): Boolean = {
    s match {
      case (s, "http://www.wikidata.org/entity/P31", o) => true
      case (s, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", o) => true
      case _ => false
    }
  }

  def getTypes : List[String] = {
    return statementsList.filter((s) => isType(s)).map(_._3)
  }
  def findScalingFactor(graph: GraphRDF) : Double = {
    val otherEntity = graph.entity
    var overlaps = 0
    for(statement <- graph.statements) {
      statement match {
        case (`otherEntity`, p, value) => if(getProperties(s=true).exists(_ == p)) overlaps += 1
        case (value, p, `otherEntity`) => if(getProperties(o=true).exists(_ == p)) overlaps += 1
      }
    }
    return overlaps.toDouble /  graph.statements.size
  }
  def getProperties(s : Boolean = false, o : Boolean = false) : List[String] = {
    val properties = ArrayBuffer[String]()
    statements.foreach((f) => if(!s && !o || s && f._1 == entity || o && f._3 == entity) properties.append(f._2))
    return properties.toList
  }

  val tooCommonProperties = Set("P31")
//  val tooCommonProperties = Set("P31")

  def getUniqueWikidataPropertiesWithoutTheMostCommon() : Set[String] = {
    return Set() ++ statementsList.filterNot((s) => (s._2.indexOf("P") > 0) && tooCommonProperties.contains(s._2.substring(s._2.indexOf("P")))).
      map((s) => s._2)
  }

}
