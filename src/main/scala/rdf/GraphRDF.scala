package rdf

import query.specific.QueryFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 09.11.2016.
  */
class GraphRDF(val entity : String) {


  val entityIsObjectStatements = QueryFactory.findSubjectsAndProperties(entity)
  val statements = mutable.Set[Tuple3[String, String, String]]()
  for((subject, property) <- entityIsObjectStatements(0) zip entityIsObjectStatements(1)) {
    statements.add((subject, property, entity))
  }
  val entityIsSubjectStatments = QueryFactory.findPropertiesAndObjects(entity)
  for((property, objectValue) <- entityIsSubjectStatments(0) zip entityIsSubjectStatments(1)) {
    statements.add((entity, property, objectValue))
  }

  def getProperties(s : Boolean = false, o : Boolean = false) : List[String] = {
    val properties = ArrayBuffer[String]()
    statements.foreach((f) => if(!s && !o || s && f._1 == entity || o && f._3 == entity) properties.append(f._2))
    return properties.toList
  }

}
