package core.query.specific

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{KnowledgeGraph, MyDatasets, PrimitiveDatatype, SimilarPropertyOntology}
import core.query.Query

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * Created by espen on 17.02.17.
  */
object QueryFactory {
  def findIsDescriptive()(implicit knowledgeGraph: KnowledgeGraph) : (List[String], List[Boolean]) = {
    val queryString =
      s"""
         |SELECT *
         |WHERE {
         |  ?property <${SimilarPropertyOntology.isDescriptive}> ?isDescriptive
         |}
        """.stripMargin
    val query = executeQuery(queryString, KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph))
    val properties = query.getResults("property")
    val strategies = query.getResults("isDescriptive")

    return (properties,strategies)
  }

  def findDistinctPropertiesWhereObject(objectValue: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT distinct ?p
         |WHERE {
         |  ?s ?p <$objectValue>
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("p")
  }
  def findSubjectsOfProperty(property: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <$property> ?o
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("s")
  }
  def findObjectsOfProperty(property: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?o
         |WHERE {
         |  ?s <$property> ?o
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("o")
  }


  def singleSubjectWithPropertyAndValue(musicbrainzIdPropertyName: String, objectValue: String)(implicit knowledgeGraph: KnowledgeGraph) : String = {
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <$musicbrainzIdPropertyName> "$objectValue"
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("s")(0)
  }

  def findAllDomainCounts()(implicit knowledgeGraph: KnowledgeGraph) : (List[String], List[Int]) = {
    val queryString =
      s"""
         |SELECT *
         |WHERE {
         |  ?property <${SimilarPropertyOntology.domainCount}> ?domainCount
         |}
        """.stripMargin
    val query = executeQuery(queryString, KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph))
    val properties = query.getResults("property")
    val strategies = query.getResults("domainCount")

    return (properties,strategies)
  }
  def findAllRangeCounts()(implicit knowledgeGraph: KnowledgeGraph) : (List[String], List[Int]) = {
    val queryString =
      s"""
         |SELECT *
         |WHERE {
         |  ?property <${SimilarPropertyOntology.rangeCount}> ?domainCount
         |}
        """.stripMargin
    val query = executeQuery(queryString, KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph))
    val properties = query.getResults("property")
    val strategies = query.getResults("domainCount")

    return (properties,strategies)
  }

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


  def objectsOfTypeWithPropertyAndSubject(property: String, subject: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph): List[String] = {
    val queryString =
      s"""
         |SELECT ?o
         |WHERE {
         |  <$subject> <$property> ?o .
         |${QueryHelper.getSameTypeFilterForQueryVariable("o", rdfTypes)}
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    val objects = query.getResults("o")
    return objects
  }
  def objectsWithPropertyAndSubject(property: String, subject: String)(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?o
         |WHERE {
         |  <$subject> <$property> ?o .
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    val objects = query.getResults("o")
    return objects

  }
  def subjectsOfTypeWithPropertyAndValue(property: String, objectValue: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <$property> <$objectValue> .
         |${QueryHelper.getSameTypeFilterForQueryVariable("s", rdfTypes)}
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    val subjects = query.getResults("s")
    return subjects
  }

  def subjectsWithPropertyAndValue(property: String, value: String)(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <$property> <$value> .
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    val subjects = query.getResults("s")
    return subjects
  }

  def findObjectsOfTypeForProperty(property: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph)  : List[String] = {
    val queryString =
      s"""
         |SELECT ?o
         |WHERE {
         |  ?s <$property> ?o .
         |${QueryHelper.getSameTypeFilterForQueryVariable("o", rdfTypes)}
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    val objects = query.getResults("o")
    return objects
  }

  def findSubjectsOfTypeForProperty(property: String, rdfTypes: List[String])(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <$property> ?v .
         |${QueryHelper.getSameTypeFilter("s", rdfTypes)}
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    val subjects = query.getResults("s")
    return subjects
  }


  def getValueMatchFromExistingDb(value: String, property: String)(implicit knowledgeGraph : KnowledgeGraph): Option[Int] = {
    val queryString =
      s"""
         |SELECT ?c
         |WHERE {
         |  <$property> <${SimilarPropertyOntology.valueMatchProperty}> ?o.
         |  ?o <${SimilarPropertyOntology.valueMatchValue}> <$value> .
         |  ?o <${SimilarPropertyOntology.valueMatchCount}> ?c .
         |}
        """.stripMargin
    val query: Query = executeQuery(queryString)
    try {
      return Some(query.getResults("c")(0))
    }
    catch {
      case a: Throwable => return None
    }
  }
  def findCountForPropertyWithSubject(property: String, subject: String)(implicit knowledgeGraph : KnowledgeGraph) : Try[Int] = {
    val queryString =
      s"""
         |SELECT (count(?object) as ?c)
         |WHERE {
         |  <$subject> <$property> ?object
         |}
        """.stripMargin
    val query: Query = executeQuery(queryString)
    Try(query.getResults("c")(0))
  }
  def findCountForPropertyWithValue(property: String, objectValue: String)(implicit knowledgeGraph : KnowledgeGraph) : Try[Int] = {
    val queryString =
      s"""
         |SELECT (count(?subject) as ?c)
         |WHERE {
         |  ?subject <$property> <$objectValue>
         |}
        """.stripMargin
    val query: Query = executeQuery(queryString)
    Try(query.getResults("c")(0))
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
  def findIdDBpediaFromWikidataId(wikidataId : String): Try[String] = {
    val queryString =
      s"""
         |select ?s
         |where {
         |  ?s <${SimilarPropertyOntology.owlSameAs}> <$wikidataId>
         |  }
       """.stripMargin
    val query = executeQuery(queryString, MyDatasets.interlinkDBpediaWikidata)(null)
    return Try(query.getResults("s")(0))
  }
  def findIdWikidataFromDBpedia(dbPediaId: String): Try[String] = {
    val queryString =
      s"""
         |select ?o
         |where {
         |  <$dbPediaId> <${SimilarPropertyOntology.owlSameAs}> ?o
         |  }
       """.stripMargin
    val query = executeQuery(queryString, MyDatasets.interlinkDBpediaWikidata)(null)
    return Try(query.getResults("o")(0))
  }
  def findObjectsHavingMoreThanXStatementsOfPropertyWithType(countThreshold: Int, property : String, subjectType: String)(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val typeProperty = KnowledgeGraph.getTypeProperty(knowledgeGraph)
    val subclassProperty = KnowledgeGraph.getSubclassProperty(knowledgeGraph)
    val queryString =
      s"""
        |select ?o (count(?s) as ?c)
        |where {
        |  ?s <$property> ?o .
        |  #?s <$typeProperty>|(<$typeProperty>/<$subclassProperty>) <$subjectType> .
        |  ?s <$typeProperty> ?oT .
        |  ?oT <$subclassProperty> ?sT .
        |  filter(?oT = <$subjectType> || ?sT = <$subjectType>)
        |}
        |Group by ?o
        |Having (?c > $countThreshold)
      """.stripMargin
    return executeQuery(queryString).getResults("o")
  }
  def findObjectsHavingMoreThanXStatementsOfPropertyWhereSubjectsAgainHaveObjectValue(thresholdCount : Int, property : String, subjectPropertyValuePair : (String, String))(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
        |SELECT ?o (COUNT(?s) AS ?c) WHERE   {
        |  ?s <$property> ?o .
        |  ?s <${subjectPropertyValuePair._1}> <${subjectPropertyValuePair._2}>.
        |} GROUP BY ?o HAVING ( ?c > ${thresholdCount} ) Order by desc(?c)
      """.stripMargin
    return executeQuery(queryString).getResults("o")
  }
  def findSubjectsHavingMoreThanXStatementsOfPropertyWhereSubjectsAgainHaveObjectValue(thresholdCount: Int, property: String, subjectPropertyValuePair: (String, String))(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?s (COUNT(?o) AS ?c) WHERE   {
         |  ?s <$property> ?o .
         |  ?o <${subjectPropertyValuePair._1}> <${subjectPropertyValuePair._2}>.
         |} GROUP BY ?s HAVING ( ?c > ${thresholdCount} ) Order by desc(?c)
      """.stripMargin
    return executeQuery(queryString).getResults("s")
  }
  def findMostCommonTypesOneStepUpInHierarchyForPropertyAndObject(property: String, objectEntity: String)(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val typeProperty = KnowledgeGraph.getTypeProperty(knowledgeGraph)
    val subclassProperty = KnowledgeGraph.getSubclassProperty(knowledgeGraph)
    val queryString =
      s"""
        |SELECT  ?t (COUNT(?s) AS ?c) WHERE   {
        |  ?s  <$property>  <$objectEntity> .
        |  ?s <$typeProperty>|(<$typeProperty>/<$subclassProperty>) ?t .
        |} GROUP BY ?t HAVING ( ?c > 1 ) Order by desc(?c)
        |
      """.stripMargin
    return executeQuery(queryString).getResults("t")
  }
  def findPropertyObjectPairsCountsForSubjectsHavingEntityAsObjectForProperty(property: String, objectEntity : String)(implicit knowledgeGraph : KnowledgeGraph) : List[(String, String, Int)] = {
    val prefix = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    val queryString =
      s"""
        |select ?p ?o (count(?o) as ?oc)
        |where {
        |  ?s <$property> <$objectEntity> .
        |  ?s ?p ?o .
        |  filter(strstarts(str(?o), "$prefix"))
        |}
        |Group by ?o ?p
        |Having (?oc > 1)
        |Order by desc(?oc)
      """.stripMargin
    val query = executeQuery(queryString)
    val counts : List[Int] = query.getResults("oc")
    val objects : List[String] = query.getResults("o")
    val properties : List[String] = query.getResults("p")
    return (for(i <- 0 until objects.size)yield(properties(i), objects(i), counts(i))).toList
  }
  def findPropertyObjectPairsCountsForObjectsHavingEntityAsSubjectForProperty(property: String, subject : String)(implicit knowledgeGraph : KnowledgeGraph) : List[(String, String, Int)] = {
    val prefix = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    val queryString =
      s"""
         |select ?p ?o (count(?o) as ?oc)
         |where {
         |  <$subject> <$property> ?oE .
         |  ?oE ?p ?o .
         |  filter(strstarts(str(?o), "$prefix"))
         |}
         |Group by ?o ?p
         |Having (?oc > 1)
         |Order by desc(?oc)
      """.stripMargin
    val query = executeQuery(queryString)
    val counts : List[Int] = query.getResults("oc")
    val objects : List[String] = query.getResults("o")
    val properties : List[String] = query.getResults("p")
    return (for(i <- 0 until objects.size)yield(properties(i), objects(i), counts(i))).toList
  }




}
