package core.query.specific

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{KnowledgeGraphs, MyDatasets, PrimitiveDatatype, SimilarPropertyOntology}
import core.query.Query

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * Created by espen on 17.02.17.
  */
object QueryFactory {

  def findObjectsOfPropertyWhereCountGreaterThanThreshold(property: String, thresholdStoreValueMatchCount : Int)(implicit knowledgeGraph: KnowledgeGraph) : List[(String, Int)] = {
    val queryString =
      s"""
         |select ?o (count(?s) as ?c)
         |where {
         |  ?s <$property> ?o .
         |  }
         |Group by ?o
         |Having(?c > ${thresholdStoreValueMatchCount})
       """.stripMargin
    val query = executeQuery(queryString)
    val objects : List[String] = query.getResults("o")
    val counts : List[Int]= query.getResults("c")
    return objects.zip(counts)
  }

  def findLowCountPropertiesWhereEntityIsObject(entity : String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryStringHighCount =
      s"""
         |SELECT distinct ?p
         |WHERE {
         |  ?p <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchProperty> ?b .
         |  ?b <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchValue> <$entity> .
         |  ?b <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchCount> ?c
         |  filter(strlen(?c) > 4)
         |}
       """.stripMargin
    val queryHighCount = executeQuery(queryStringHighCount)
    val highCount : List[String]= queryHighCount.getResults("p")
    val filterLine = if(highCount.isEmpty) "" else s"filter(?p not in (<${highCount.mkString(">, <")}>))"
    val queryString =
      s"""
         |SELECT distinct ?p
         |WHERE {
         |  ?s ?p <$entity> .
         |  $filterLine
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("p")
  }

  def findOrderedCountForTypes(isDomainProperties: List[String], isRangeProperties: List[String], numberOfComparableTypes : Int=10)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val typesIsDomainOfProperties = isDomainProperties.map(p => s"{?s <${SimilarPropertyOntology.isDomainType}> <$p> }").mkString("UNION")
    val typesIsRangeOfProperties = isRangeProperties.map(p => s"{?s <${SimilarPropertyOntology.isRangeType}> <$p> }").mkString("UNION")
    val unionWithRange = if(typesIsRangeOfProperties.isEmpty) "" else s"${if(typesIsDomainOfProperties.isEmpty) typesIsRangeOfProperties else s"UNION $typesIsRangeOfProperties"}"
    val queryString =
      s"""
         |select ?s (count(?s) as ?c)
         |where {
         |  $typesIsDomainOfProperties $unionWithRange
         |}Group by ?s
         |Order by desc(?c)
         |LIMIT $numberOfComparableTypes
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("s")
  }

  def findRangeTypesForPropertyLocally(property: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <${SimilarPropertyOntology.isRangeType}> <$property> .
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("s")
  }

  def findDomainTypesForPropertyLocally(property: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <${SimilarPropertyOntology.isDomainType}> <$property> .
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("s")
  }


  def findDomainTypesForProperty(property: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT distinct ?t
         |WHERE {
         |  ?s <$property> ?o .
         |  ?s <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?t
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("t")
  }
  def findRangeTypesForProperty(property: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString =
      s"""
         |SELECT distinct ?t
         |WHERE {
         |  ?s <$property> ?o .
         |  ?o <${KnowledgeGraphs.getTypeProperty(knowledgeGraph)}> ?t
         |}
        """.stripMargin
    val query = executeQuery(queryString)
    return query.getResults("t")
  }
  def findIsDescriptive()(implicit knowledgeGraph: KnowledgeGraph) : (List[String], List[Boolean]) = {
    val queryString =
      s"""
         |SELECT *
         |WHERE {
         |  ?property <${SimilarPropertyOntology.isDescriptive}> ?isDescriptive
         |}
        """.stripMargin
    val query = executeQuery(queryString)
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
         |  filter(isUri(?property))
         |}
        """.stripMargin
    val query = executeQuery(queryString)
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
         |  filter(isUri(?property))
         |}
        """.stripMargin
    val query = executeQuery(queryString)
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
    val query = executeQuery(queryString)
    val properties = query.getResults("property")
    val strategies = query.getResults("strategy")

    return (properties,strategies)
  }

  def objectsOfTypeWithPropertyAndSubject(property: String, subject: String, rdfTypes: List[String], useMustHaveProperty : Boolean = false)(implicit knowledgeGraph : KnowledgeGraph, mustHaveProperty: String="" , mustHavePropertyIsSubject : Boolean=true): List[String] = {
    val extraFilter = findExtraFilter(useMustHaveProperty, "o")
    val queryString =
      s"""
         |SELECT ?o
         |WHERE {
         |  <$subject> <$property> ?o .
         |  $extraFilter
         |${QueryHelper.getSameTypeFilterForQueryVariable("o", rdfTypes)}
         |}
         |Group by ?o
        """.stripMargin
    val query = executeQuery(queryString)
    val objects = query.getResults("o")
    return objects
  }

  def findExtraFilter(useMustHaveProperty: Boolean, queryVariable : String)(implicit mustHaveProperty: String, mustHavePropertyIsSubject : Boolean) : String = {
    if(!useMustHaveProperty) return ""
    if(mustHaveProperty == "") return ""
    if(mustHavePropertyIsSubject) s"?$queryVariable <${mustHaveProperty}> ?dontMatter ."
    else {
      s"?dontMatter <$mustHaveProperty> ?$queryVariable ."
    }

  }

  def objectsWithPropertyAndSubject(property: String, subject: String, useMustHaveProperty : Boolean =false)(implicit knowledgeGraph : KnowledgeGraph, mustHaveProperty: String="" , mustHavePropertyIsSubject : Boolean=true) : List[String] = {
    val extraFilter = findExtraFilter(useMustHaveProperty, "o")
    val queryString =
      s"""
         |SELECT ?o
         |WHERE {
         |  <$subject> <$property> ?o .
         |  $extraFilter
         |}
         |Group by ?o
        """.stripMargin
    val query = executeQuery(queryString)
    val objects = query.getResults("o")
    return objects

  }

  def subjectsOfTypeWithPropertyAndValue(property: String, objectValue: String, rdfTypes: List[String], useMustHaveProperty : Boolean= false)(implicit knowledgeGraph : KnowledgeGraph, mustHaveProperty: String="", mustHavePropertyIsSubject : Boolean=true) : List[String] = {
    val extraFilter = findExtraFilter(useMustHaveProperty, "s")
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <$property> <$objectValue> .
         |  $extraFilter
         |${QueryHelper.getSameTypeFilterForQueryVariable("s", rdfTypes)}
         |}
         |Group by ?s
        """.stripMargin
    val query = executeQuery(queryString)
    val subjects = query.getResults("s")
    return subjects
  }

  def subjectsWithPropertyAndValue(property: String, value: String, useMustHaveProperty : Boolean =false)(implicit knowledgeGraph : KnowledgeGraph, mustHaveProperty: String="" , mustHavePropertyIsSubject : Boolean=true) : List[String] = {

    val extraFilter = findExtraFilter(useMustHaveProperty, "s")
    val queryString =
      s"""
         |SELECT ?s
         |WHERE {
         |  ?s <$property> <$value> .
         |  $extraFilter
         |}
         |Group by ?s
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
    if(queryString.contains("<http://www.wikidata.org/entity/P31>  <http://www.wikidata.org/entity/Q1800833>")) {
      val a = 5
    }
    val query: Query = executeQuery(queryString)
    Try(query.getResults("c")(0))
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  def findPropertiesAndObjectsFuture(subject: String):Future[Tuple2[List[String], List[String]]] = {
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
    return Future {
      println("Executing future...")
      findPropertiesAndObjects(subject)
    }
  }
  def findPropertiesAndObjectsFuture2(subject: String)(implicit ec: ExecutionContext):Future[Int] = {
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
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
    val typeProperty = KnowledgeGraphs.getTypeProperty(knowledgeGraph)
    val subclassProperty = KnowledgeGraphs.getSubclassProperty(knowledgeGraph)
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
    val typeProperty = KnowledgeGraphs.getTypeProperty(knowledgeGraph)
    val subclassProperty = KnowledgeGraphs.getSubclassProperty(knowledgeGraph)
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
    val prefix = KnowledgeGraphs.getDatasetEntityPrefix(knowledgeGraph)
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
    val prefix = KnowledgeGraphs.getDatasetEntityPrefix(knowledgeGraph)
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
