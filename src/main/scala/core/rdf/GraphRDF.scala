package core.rdf

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.SimilarPropertyOntology
import core.query.specific.QueryFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 09.11.2016.
  */
class GraphRDF(val entity : String)(implicit val knowledgeGraph: KnowledgeGraph) {
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

  val entityIsObjectStatements = QueryFactory.findSubjectsAndProperties(entity)
  val statements = mutable.Set[Tuple3[String, String, String]]()
  for((subject, property) <- entityIsObjectStatements._1 zip entityIsObjectStatements._2) {
    statements.add((subject, property, entity))
  }
  val entityIsSubjectStatments = QueryFactory.findPropertiesAndObjects(entity)
  for((property, objectValue) <- entityIsSubjectStatments._2 zip entityIsSubjectStatments._1) {
    statements.add((entity, property, objectValue))
  }
  val statementsList = statements.toList
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
