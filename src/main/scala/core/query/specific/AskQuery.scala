package core.query.specific

import core.globals.KnowledgeGraph.KnowledgeGraph
import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 08.11.2016.
  */
object AskQuery {
  def subjectHasType(subject: String, rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |WHERE {
         |  <$subject> <wd:P31> <$rdfTypes>.
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }
  def sharableRange(prop: String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
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
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |WHERE {
         |  ?s <$prop> ?object .
         |  ?s wd:P31 ?t .
         |  ?object wd:P31 ?t .
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))

  }

  def ask(f : () => String)(implicit knowledgeGraph: KnowledgeGraph) : Boolean = {
    return QueryLocalServer.ask(QueryLocalServer.convertToMutlipleGraphQueryWithoutSelect(f()), DatasetInferrer.getDataset(f()))
  }


}
