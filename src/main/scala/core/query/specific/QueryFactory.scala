package core.query.specific

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{KnowledgeGraph, PrimitiveDatatype}
import core.query.Query

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by espen on 17.02.17.
  */
object QueryFactory {

  def findAllStrategies()(implicit knowledgeGraph: KnowledgeGraph) : (List[String], List[String]) = {
    val queryString =
      s"""
         |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |SELECT *
         |WHERE {
         |  ?property rdf:type ?strategy
         |}
        """.stripMargin
    val query = executeQuery(queryString, KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph))
    val properties = query.getResults("property")
    val strategies = query.getResults("strategy")

    return (properties,strategies)
  }

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

  import scala.concurrent.ExecutionContext.Implicits.global
  def findPropertiesAndObjectsFuture(subject: String):Future[Tuple2[List[String], List[String]]] = {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    return Future {
      println("Executing future...")
      findPropertiesAndObjects(subject)
    }
  }
  def findPropertiesAndObjectsFuture2(subject: String)(implicit ec: ExecutionContext):Future[Int] = {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
  import scala.concurrent.ExecutionContext.Implicits.global

  return Future {
      println("Executing future...")
      1000
//      findPropertiesAndObjects(subject)
    }
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

  private def executeQuery(queryString: String, datasetForced: String = "")(implicit knowledgeGraph : KnowledgeGraph): Query = {
    val query = new Query(() => queryString, if(datasetForced != "") datasetForced else DatasetInferrer.getDataset(queryString))
    query.execute()
    query
  }




}
