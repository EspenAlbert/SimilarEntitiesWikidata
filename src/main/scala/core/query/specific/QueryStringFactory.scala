package core.query.specific

import core.globals.{KnowledgeGraphs, SimilarPropertyOntology}
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.QueryFactory.executeQuery

/**
  * Created by espen on 02.05.17.
  */
object QueryStringFactory {
  def propertiesWhereDomainHasType(typeEntity: String) : String =
    s"""
       |select ?p
       |where {
       |  <$typeEntity> <${SimilarPropertyOntology.isDomainType}> ?p
       |}
     """.stripMargin

  def propertiesWhereRangeHasType(typeEntity: String) : String =
    s"""
       |select ?p
       |where {
       |  <$typeEntity> <${SimilarPropertyOntology.isRangeType}> ?p
       |}
     """.stripMargin

  def countEntitiesOfTypeForProperty(typeEntity: String, isSubject: Boolean, property: String)(implicit knowledgeGraph: KnowledgeGraph) : String =
    s"""
       |select (count(distinct ?e) as ?c)
       |where {
       |  ${if(isSubject) "?e" else "?s"} <$property> ${if(!isSubject) "?e" else "?o"} .
       |  ?e <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> <$typeEntity> .
       |}
     """.stripMargin

  def possibleTypes: String =
    s"""
       |select distinct ?pT
       |where {
       |  ?pT <${SimilarPropertyOntology.isDomainType}> ?o
       |}
     """.stripMargin

  def hiearchyLevel(entityType: String) =
    s"""
       |select ?hL
       |where {
       |<$entityType> <${SimilarPropertyOntology.hierarchyLevel}> ?hL
       |}
     """.stripMargin

  def childrenOf(parent: String)(implicit knowledgeGraph: KnowledgeGraph) : String =
    s"""
       |select ?c
       |where {
       |  ?c <${KnowledgeGraphs.getSubclassProperty(knowledgeGraph)}> <$parent>
       |}
     """.stripMargin

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
