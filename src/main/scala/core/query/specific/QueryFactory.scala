package core.query.specific

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{PrimitiveDatatype}
import core.query.Query

/**
  * Created by espen on 17.02.17.
  */
object QueryFactory {
  def singleSubjectWithPropertyAndValue(property: String, objectValue: String)(implicit knowledgeGraph : KnowledgeGraph) : String ={
    throw new NotImplementedError()
}

  def objectsOfTypeWithPropertyAndSubject(property: String, subject: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph): List[String] = {
    throw new NotImplementedError()
  }
  def subjectsOfTypeWithPropertyAndValue(property: String, objectValue: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    throw new NotImplementedError()
  }


  def findObjectsOfTypeForProperty(property: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph)  : List[String] = {
    throw new NotImplementedError()
  }

  def findSubjectsOfTypeForProperty(property: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    throw new NotImplementedError()

  }

  def findDistinctCountForPropertyWithSubject(property: String, subject: String)(implicit knowledgeGraph : KnowledgeGraph) : Option[Int] = {
    throw new NotImplementedError()

  }

  def getValueMatchFromExistingDb(value: String, property: String)(implicit knowledgeGraph : KnowledgeGraph): Option[Int] = {
    throw new NotImplementedError()

    //    try {
//      val statement1 = SimpleRDFFactory.getStatement(property, SimilarPropertyOntology.valueMatchProperty, "?o " + OptionsForResultQueryVariable.ignoreMe)
//      val statement2 = SimpleRDFFactory.getStatement("?o " + OptionsForResultQueryVariable.ignoreMe, SimilarPropertyOntology.valueMatchValue, value)
//      val statement3 = SimpleRDFFactory.getStatement("?o " + OptionsForResultQueryVariable.ignoreMe, SimilarPropertyOntology.valueMatchCount, "?c")
//      val count = QueryFactoryV2.findSingleValue(statement1, statement2, statement3)
//      return Some(count)
//    } catch {
//      case a: Throwable => return None
//    }
  }
  def findDistinctCountForPropertyWithValue(property: String, objectValue: String)(implicit knowledgeGraph : KnowledgeGraph) : Option[Int] = {
    throw new NotImplementedError()
  }

  def findPropertiesAndObjects(subject: String)(implicit knowledgeGraph : KnowledgeGraph) : Tuple2[List[String], List[String]] = {
    val queryString =
      s"""
         |SELECT ?o ?p
         |WHERE {
         |  <$subject> ?p ?o.
         |}
        """.stripMargin
    val query: Query = executeQuery(queryString)
    return Tuple2(query.getResults("o"), query.getResults("p"))
  }
  def findSubjectsAndProperties(objectEntity: String)(implicit knowledgeGraph : KnowledgeGraph) : Tuple2[List[String], List[String]] = {
    val queryString =
      s"""
         |SELECT ?s ?p
         |WHERE {
         |  ?s ?p <$objectEntity>.
         |}
        """.stripMargin
    val query: Query = executeQuery(queryString)
    return Tuple2(query.getResults("s"), query.getResults("p"))
  }

  def findDomainCount(prop: String)(implicit knowledgeGraph : KnowledgeGraph) : Int = {
    val queryString =
      s"""
         |SELECT (count(distinct ?s) as ?c)
         |WHERE {
         |  ?s <$prop> ?object .
         |}
        """.stripMargin
    val query: Query = executeQuery(queryString)
    return query.getResults("c")(0)
  }
  def findRangeCount(prop: String)(implicit knowledgeGraph : KnowledgeGraph) : Int ={
    val queryString =
      s"""
         |SELECT (count(distinct ?object) as ?c)
         |WHERE {
         |  ?s <$prop> ?object .
         |}
        """.stripMargin
    val query: Query = executeQuery(queryString)
    return query.getResults("c")(0)
  }
  def findMaxDate(prop: String)(implicit knowledgeGraph : KnowledgeGraph) : Option[Int] = {
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
    val query: Query = executeQuery(queryString)
    return PrimitiveDatatype.getYearFromDateFormat(query.getResults("maxDate")(0))
  }

  def findAllDistinctProperties(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT distinct ?p
         |WHERE {
         |  ?s ?p ?o .
         |}
      """.stripMargin
    val query: Query = executeQuery(queryString)
    return query.getResults("p")
  }


  //Always expectign full URI without <>  for property
  def findAllDistinctDatatypesForProperty(property : String)(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
        |SELECT distinct ?b
        |WHERE {
        |  ?s <$property> ?object .
        |  bind(datatype(?object) as ?b)
        |}
      """.stripMargin
    val query: Query = executeQuery(queryString)
    return query.getResults("b")
  }
  def find100SamplesForProperty(property : String)(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |select ?o
         |where {
         |  ?s <$property> ?o
         |  }
         |  LIMIT 100
       """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("o")
  }

  private def executeQuery(queryString: String)(implicit knowledgeGraph : KnowledgeGraph): Query = {
    val query = new Query(() => queryString, DatasetInferrer.getDataset(queryString))
    query.execute()
    query
  }




}
