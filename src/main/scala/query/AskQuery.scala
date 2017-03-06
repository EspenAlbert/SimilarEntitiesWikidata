package query

import jenaQuerier.QueryLocalServer
import query.specific.DatasetInferrer

/**
  * Created by Espen on 08.11.2016.
  */
object AskQuery {
  def sharableRange(prop: String) : Boolean = {
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |WHERE {
         |  ?s1 <$prop> ?o .
         |  ?s2 <$prop> ?o .
         |  filter(?s1 != ?s2)
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }

  def sharableDomain(prop: String) : Boolean = {
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |WHERE {
         |  ?s <$prop> ?o1 .
         |  ?s <$prop> ?o2 .
         |  filter(?o1 != ?o2)
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))
  }

  def sameTypePossibleForProp(prop: String) : Boolean = {
    val askQuery =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |ask
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |WHERE {
         |  ?s <$prop> ?object .
         |  ?s wd:P31 ?t .
         |  ?object wd:P31 ?t .
         |}
        """.stripMargin
    return QueryLocalServer.ask(askQuery, DatasetInferrer.getDataset(askQuery))

  }

  def ask(f : () => String) : Boolean = {
    return QueryLocalServer.ask(QueryLocalServer.convertToMutlipleGraphQueryWithoutSelect(f()), DatasetInferrer.getDataset(f()))
  }


}
