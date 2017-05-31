package core.query.specific

import core.globals.{KnowledgeGraphs, SimilarPropertyOntology}
import core.globals.KnowledgeGraphs.KnowledgeGraph
import jenaQuerier.QueryLocalServer

import scala.util.{Failure, Success, Try}

/**
  * Created by Espen on 08.11.2016.
  */
object AskQuery {
//  def domainHasType(property: String, typeEntity: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
//    val qString =
//      s"""
//         |ask
//         |where {
//         |  ?s <$property> ?o
//         |  ?s <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> <${typeEntity}>
//       """.stripMargin
//    val ds = DatasetInferrer.getDataset()
//  }

  def existsValueMatchForProperty(property: String)(implicit knowledgeGraph: KnowledgeGraph): Boolean = {
    val askQuery =
      s"""
         |ask
         |WHERE {
         |  <$property> <${SimilarPropertyOntology.valueMatchProperty}> ?o .
         |  ?o <${SimilarPropertyOntology.valueMatchCount}> ?c .
         |  filter(?c > 1000)
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }

  def isParentOfChild(potentialParent: String, child: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val askQuery = s"ask where { <$child> <${KnowledgeGraphs.getSubclassProperty(knowledgeGraph)}> <$potentialParent> }"
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }
  def oneOfTypesUsedInDomainOrRange(comparableTypes: List[String], property: String, isSubject: Boolean, isObject: Boolean)(implicit knowledgeGraph: KnowledgeGraph): (Boolean, Boolean) = {
    val queryStringUsedInDomain = if(isSubject && comparableTypes.nonEmpty) AskQueryStringFactory.oneOfTypesUsedInDomain(comparableTypes, property) else ""
    val queryStringUsedInRange = if(isObject && comparableTypes.nonEmpty) AskQueryStringFactory.oneOfTypesUsedInRange(comparableTypes, property) else ""
    if(queryStringUsedInDomain + queryStringUsedInRange == "") return (false, false)
    val dataset = DatasetInferrer.getDataset(queryStringUsedInDomain + queryStringUsedInRange)
    return (queryStringUsedInDomain.nonEmpty && QueryLocalServer.ask(queryStringUsedInDomain, dataset),queryStringUsedInRange.nonEmpty && QueryLocalServer.ask(queryStringUsedInRange, dataset))
  }
  def maxCountSameSubject(property: String)(implicit knowledgeGraph: KnowledgeGraph): Boolean = {
    val askQuery =
      s"""
         |ask {select ?o (COUNT(?s) AS ?c)
         |WHERE   {
         |  ?s  <$property>  ?o .
         |}
         |GROUP BY ?o
         |HAVING ( ?c > 99 )
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }

  def subjectHasType(subject: String, rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    if(!subject.startsWith("http")) return false
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |WHERE {
         |<$subject> <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?v
         |${QueryHelper.getSameTypeFilter("v", rdfTypes)}
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }
  def sharableRange(prop: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val typeProperty = KnowledgeGraphs.getTypeProperty(knowledgeGraph)
    if(typeProperty == prop)return true
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |WHERE {
         |  ?s1 <$prop> ?o .
         |  ?s2 <$prop> ?o .
         |  filter(?s1 != ?s2)
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }

  def sharableDomain(prop: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |WHERE {
         |  ?s <$prop> ?o1 .
         |  ?s <$prop> ?o2 .
         |  filter(?o1 != ?o2)
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }

  def sameTypePossibleForProp(prop: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val typeProperty = KnowledgeGraphs.getTypeProperty(knowledgeGraph)
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |WHERE {
         |  ?s <$prop> ?object .
         |  ?s <${typeProperty}> ?t .
         |  ?object <${typeProperty}> ?t .
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))

  }
  def isItemProperty(property: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val itemPropertyQuery =
      s"""
         |ask
         |where {
         | ?s <$property> ?o .
         | ?o ?p ?v .
         | }
      """.stripMargin
    return QueryLocalServer.ask(itemPropertyQuery, DatasetInferrer.getDataset(itemPropertyQuery))

  }

  def ask(f : () => String, ds : String = "")(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val dataset = if(ds!= "") ds else DatasetInferrer.getDataset(f())
    return QueryLocalServer.ask(s"ask WHERE {${f()} }", dataset)
  }
  def datasetIsEmpty(ds: String) : Boolean = {
    return !QueryLocalServer.ask("ask where {?s ?p ?o}", ds)
  }


}
