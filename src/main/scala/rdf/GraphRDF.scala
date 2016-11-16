package rdf

import query.specific.QueryFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
/**
  * Created by Espen on 09.11.2016.
  */
class GraphRDF(val entity : String) {
  var getType: String = ""


  val entityIsObjectStatements = QueryFactory.findSubjectsAndProperties(entity)
  val statements = mutable.Set[Tuple3[String, String, String]]()
  for((subject, property) <- entityIsObjectStatements(0) zip entityIsObjectStatements(1)) {
    statements.add((subject, property, entity))
  }
  val entityIsSubjectStatments = QueryFactory.findPropertiesAndObjects(entity)
  for((property, objectValue) <- entityIsSubjectStatments(0) zip entityIsSubjectStatments(1)) {
    statements.add((entity, property, objectValue))
  }
  val statementsList = statements.toList
  def isType(s: (String, String, String)): Boolean = {
    s match {
      case (s, "http://www.wikidata.org/entity/P31", o) => true
      case _ => false
    }
  }

  getType = statementsList.find((s) => isType(s)).get._3
  def getProperties(s : Boolean = false, o : Boolean = false) : List[String] = {
    val properties = ArrayBuffer[String]()
    statements.foreach((f) => if(!s && !o || s && f._1 == entity || o && f._3 == entity) properties.append(f._2))
    return properties.toList
  }

  val tooCommonProperties = Set("P31", "P17")

  def getUniqueWikidataPropertiesWithoutTheMostCommon() : Set[String] = {
    return Set() ++ statementsList.filterNot((s) => (s._2.indexOf("P") > 0) && tooCommonProperties.contains(s._2.substring(s._2.indexOf("P")))).
      map((s) => s._2)
  }

}
