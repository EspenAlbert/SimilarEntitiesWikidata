package query.specific

import globals.PrimitiveDatatype
import query.Query
import query.variables.DynamicQueryVariable

/**
  * Created by espen on 17.02.17.
  */
object QueryFactoryRaw {
  def findDomainCount(prop: String) : Int = {
    val queryString =
      s"""
         |SELECT (count(distinct ?s) as ?c)
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |WHERE {
         |  ?s <$prop> ?object .
         |}
        """.stripMargin
    val query = new Query(() => queryString, DatasetInferrer.getDataset(queryString))
    query.execute()
    return query.getResults(new DynamicQueryVariable("c"))(0)
  }
  def findRangeCount(prop: String) : Int ={
    val queryString =
      s"""
         |SELECT (count(distinct ?object) as ?c)
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |WHERE {
         |  ?s <$prop> ?object .
         |}
        """.stripMargin
    val query = new Query(() => queryString, DatasetInferrer.getDataset(queryString))
    query.execute()
    return query.getResults(new DynamicQueryVariable("c"))(0)
  }
  def findMaxDate(prop: String) : Option[Int] = {
    val queryString =
      s"""
         |SELECT (MAX(?date) AS ?maxDate)
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |WHERE {
         |  ?subject <$prop> ?date
         |}
       """.stripMargin
    val query = new Query(() => queryString, DatasetInferrer.getDataset(queryString))
    query.execute()
    return PrimitiveDatatype.getYearFromDateFormat(query.getResults(new DynamicQueryVariable("maxDate"))(0))
  }

  def findAllDistinctProperties : List[String] = {
    val queryStringOld =
      s"""
         |SELECT distinct ?p
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |WHERE {
         |  ?s ?p ?object .
         |}
      """.stripMargin
    val queryString =
      s"""
         |SELECT distinct ?p
         |WHERE {
         |  ?s ?p ?object .
         |}
      """.stripMargin
    val query = new Query(() => queryString, DatasetInferrer.getDataset(queryString))
    query.execute()
    return query.getResults(new DynamicQueryVariable("p"))
  }


  //Always expectign full URI without <>  for property
  def findAllDistinctDatatypesForProperty(property : String) : List[String] = {
    val queryString =
      s"""
        |SELECT distinct ?b
        |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
        |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
        |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
        |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
        |WHERE {
        |  ?s <$property> ?object .
        |  bind(datatype(?object) as ?b)
        |}
      """.stripMargin
    val query = new Query(() => queryString, DatasetInferrer.getDataset(queryString))
    query.execute()
    return query.getResults(new DynamicQueryVariable("b"))
  }

  def find100SamplesForProperty(property : String) : List[String] = {
    val queryString =
      s"""
         |select ?o
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph1>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph2>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph3>
         |From <http://www.espenalbert.com/rdf/wikidata/localGraph4>
         |where {
         |  ?s <$property> ?o
         |  }
         |  LIMIT 100
       """.stripMargin
    val query = new Query(() => queryString, DatasetInferrer.getDataset(queryString))
    query.execute()
    return query.getResults(new DynamicQueryVariable("o"))
  }



}
