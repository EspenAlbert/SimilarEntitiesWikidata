package core.query.specific

import core.globals.{KnowledgeGraphs, SimilarPropertyOntology}
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.QueryFactory.executeQuery

/**
  * Created by espen on 02.05.17.
  */
object QueryStringFactory {
  def highValueMatchesForEntity(entity: String) : String =
    s"""
       |SELECT distinct ?p
       |WHERE {
       |  ?p <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchProperty> ?b .
       |  ?b <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchValue> <$entity> .
       |  ?b <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchCount> ?c
       |  filter(strlen(?c) > 4)
       |}
       """.stripMargin

  def propertiesAndCountsForType(typeEntity: String, thresholdCount: Int, isSubject: Boolean)(implicit knowledgeGraph: KnowledgeGraph) : String =
    s"""
       |select ?p (count(?e) as ?c)
       |where {
       |  ?e <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> <$typeEntity> .
       |  ${if(isSubject) "?e ?p ?v ." else "?v ?p ?e"}
       |} group by ?p
       |having(?c > $thresholdCount)
     """.stripMargin

  def objectsWithPropertyAndSubject(property: String, subject: String): String =
    s"""
       |select distinct ?o
       |where {
       |  <$subject> <$property> ?o .
       |}
     """.stripMargin

  def subjectsWithPropertyAndValue(property: String, objectValue: String):String =
    s"""
       |select ?s
       |where {
       |  ?s <$property> <$objectValue>
       |}
     """.stripMargin

  def countTriples() : String =
    s"""
       |select (count(?s) as ?c)
       |where {
       |?s ?p ?o
       |}
     """.stripMargin

  def selectTriples() : String =
    s"""
       |select ?s ?p ?o
       |where {
       |?s ?p ?o
       |} LIMIT 25
     """.stripMargin


  def typesWithGreaterThanCount(thresholdForStoringPropertyDistributionsLocally: Int)(implicit knowledgeGraph: KnowledgeGraph) : String =
    s"""
      select ?t (count(?s) as ?c)
       |where {
       |  ?s <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?t .
       |  }
       |Group by ?t
       |Having(?c > ${thresholdForStoringPropertyDistributionsLocally})
       |     """.stripMargin

  def domainAndRangeTypeWithCountsForProperty(property: String) : String =
    s"""
       |select ?eT ?cD ?cR
       |where {
       |  ?eT <${SimilarPropertyOntology.propertyDistributionNode}> ?pdn .
       |  ?pdn <${SimilarPropertyOntology.distributionForProperty}> <$property> .
       |  optional {?pdn <${SimilarPropertyOntology.domainCount}> ?cD .}
       |  optional {?pdn <${SimilarPropertyOntology.rangeCount}> ?cR .}
       | }
     """.stripMargin


  def propertiesForWhereEntityIsSubject(entity: String) : String = {
    s"""
       |select distinct ?p
       |where {
       |  <$entity> ?p ?o .
       |}
     """.stripMargin
  }
  def comparableTypesPropertyDistribution(domainProperties: Iterable[String], rangeProperties: Iterable[String]) : String = {
    val baseDistributionPath =
      s"""?t <${SimilarPropertyOntology.propertyDistributionNode}> ?node  .
  ?node <${SimilarPropertyOntology.distributionForProperty}> ?p .\n"""
    val domainPropertiesFilter = if(domainProperties.nonEmpty) s"{ $baseDistributionPath ?node <${SimilarPropertyOntology.domainCount}> ?dc . \n filter(?p in (<${domainProperties.mkString(">,<")}>)) }\n" else ""
    val rangePropertiesFilter = if(rangeProperties.nonEmpty) s"UNION { $baseDistributionPath ?node <${SimilarPropertyOntology.rangeCount}> ?dc . \n filter(?p in (<${rangeProperties.mkString(">,<")}>)) }" else ""
    s"""
       |SELECT ?t (count(?p) as ?c)
       |WHERE   {
       |
       |  $domainPropertiesFilter
       |  $rangePropertiesFilter
       |
       |} Group by ?t order by desc(?c)
     """.stripMargin
  }

  def propertyDistributions(entityType: String) : String = {
    s"""
       |select ?p ?cD ?cR ?tprD ?tprR
       |where {
       |  <${entityType}> <${SimilarPropertyOntology.propertyDistributionNode}> ?pdn .
       |  ?pdn <${SimilarPropertyOntology.distributionForProperty}> ?p .
       |  optional {?pdn <${SimilarPropertyOntology.domainCount}> ?cD .}
       |  optional {?pdn <${SimilarPropertyOntology.rangeCount}> ?cR .}
       |  optional {?pdn <${SimilarPropertyOntology.typePropertyRatioDomain}> ?tprD .}
       |  optional {?pdn <${SimilarPropertyOntology.typePropertyRatioRange}> ?tprR .}
       | }
     """.stripMargin
  }

  def propertyDistribution(entityType: String, property: String) : String =
    s"""
       |select ?cD ?cR ?iR
       |where {
       |  <${entityType}> <${SimilarPropertyOntology.propertyDistributionNode}> ?pdn .
       |  ?pdn <${SimilarPropertyOntology.distributionForProperty}> <$property> .
       |  ?pdn <${SimilarPropertyOntology.domainCount}> ?cD .
       |  ?pdn <${SimilarPropertyOntology.rangeCount}> ?cR .
       |  ?pdn <${SimilarPropertyOntology.typeImportanceRatio}> ?iR .
       | }
     """.stripMargin

  def typePropertyCountLocal(entityType : String, property: String, isDomain: Boolean) : String =
    s"""
       |select ?c
       |where {
       |  <${entityType}> <${SimilarPropertyOntology.propertyDistributionNode}> ?pdn .
       |  ?pdn <${SimilarPropertyOntology.distributionForProperty}> <$property> .
       |  ?pdn <${if(isDomain) SimilarPropertyOntology.domainCount else SimilarPropertyOntology.rangeCount}> ?c .
       | }
     """.stripMargin

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
  def childrenOfAndChildrenIsType(parent: String)(implicit knowledgeGraph: KnowledgeGraph) : String =
    s"""
       |select ?c
       |where {
       |  ?c <${KnowledgeGraphs.getSubclassProperty(knowledgeGraph)}> <$parent> .
       |  filter exists { ?s <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?c }
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
       |  filter(?p != <$entity>)
       |}
     """.stripMargin
  }
  def parentsToParentIsType(entity: String)(implicit knowledgeGraph: KnowledgeGraph) : String = {
    s"""
       |select ?p
       |where {
       |  <$entity> <${KnowledgeGraphs.getSubclassProperty(knowledgeGraph)}>* ?p
       |  filter(?p != <$entity>)
       |  filter exists { ?s <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?p }
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

  def parentToEntityXStepsAway(entity: String, steps: Int)(implicit knowledgeGraph: KnowledgeGraph) : String = {
    val connectingPath = Range(0, steps).map(i => {
      if (i == steps - 1) "?o"
      else
        s"?l$i . ?l$i <${KnowledgeGraphs.getSubclassProperty(knowledgeGraph)}> "
    }).mkString("")
    s"""
         |select ?o
         |where {
         |  <$entity> <${KnowledgeGraphs.getSubclassProperty(knowledgeGraph)}> $connectingPath
         |}""".stripMargin
//         |    filter exists { ?s <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?o }
  }
}
