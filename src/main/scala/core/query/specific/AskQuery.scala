package core.query.specific

import core.globals.KnowledgeGraph
import core.globals.KnowledgeGraph.KnowledgeGraph
import jenaQuerier.QueryLocalServer

import scala.util.{Failure, Success, Try}

/**
  * Created by Espen on 08.11.2016.
  */
object AskQuery {
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
         |<$subject> <${KnowledgeGraph.getTypeProperty(knowledgeGraph)}> ?v
         |${QueryHelper.getSameTypeFilter("v", rdfTypes)}
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }
  def sharableRange(prop: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val typeProperty = KnowledgeGraph.getTypeProperty(knowledgeGraph)
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |WHERE {
         |  ?s1 <$prop> ?o .
         |  ?s2 <$prop> ?o .
         |  ?o <$typeProperty> ?t .
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
    val typeProperty = KnowledgeGraph.getTypeProperty(knowledgeGraph)
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


}
