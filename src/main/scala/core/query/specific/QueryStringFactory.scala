package core.query.specific

import core.globals.KnowledgeGraphs
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.QueryFactory.executeQuery

/**
  * Created by espen on 02.05.17.
  */
object QueryStringFactory {
  def subjectsAndProperties(objectEntity: String): String = {
    s"""
       |SELECT ?s ?p
       |WHERE {
       |  ?s ?p <$objectEntity>.
       |}""".stripMargin
  }
  def parentsTo(entity: String)(implicit knowledgeGraph: KnowledgeGraph) : String = {
    s"""
       |select ?p
       |where {
       |  <$entity> <${KnowledgeGraphs.getSubclassProperty(knowledgeGraph)}>* ?p
       |}
     """.stripMargin
  }


  def distinctPropertiesWhereObject(objectValue : String) : String =
    s"""
       |SELECT distinct ?p
       |WHERE {
       |  ?s ?p <$objectValue>
       |}
     """.stripMargin

  def allTypesForEntity(entity: String)(implicit knowledgeGraph: KnowledgeGraph): String =
  s"""
     |select ?t
     |where {
     |<$entity> <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?t .
     |}
   """.stripMargin


}
