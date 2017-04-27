package similarityFinder.displayer

import core.globals.{KnowledgeGraph, MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import core.query.specific.UpdateQueryFactory
import core.query.{Query, QueryForOnlineWikidata}
import core.query.specific.UpdateQueryFactory.{datatypeDouble, datatypeInteger}
import core.query.variables.ResultVariable

import scala.util.{Failure, Success, Try}

/**
  * Created by espen on 30.03.17.
  */
object QueryFactorySimilarityResult {


  private def executeQuery(queryString: String): Query = {
    val query = new Query(() => queryString, MyDatasets.resultsSimilarArtists)
    query.execute()
    query
  }
  def findFeaturePath(runName: String, qEntity: String, similarEntity: String) : Try[String] = {
    val queryString =
      recalledQuery(runName, qEntity).init +
        s"""
           |?bR <${ResultsSimilarArtistsGlobals.featureFound}> ?fF .
           |?fF <${ResultsSimilarArtistsGlobals.featurePath}> ?fP""".stripMargin

    val correctSelect = queryString.replace("select ?r", "select ?fP")
    val correctSelectWithRecalled = correctSelect.replace("?r", s"<$similarEntity>")
    return Try{
      val path : String = executeQuery(correctSelectWithRecalled).getResults("fP")(0)
      path
    }
  }

  def findExpectedSimilars(qEntity: String) : List[String] = {
    val query =
      s"""
         |select ?sE
         |where {
         |  <$qEntity> <${ResultsSimilarArtistsGlobals.expectedSimilar}> ?sE .
         |  }
       """.stripMargin
    return executeQuery(query).getResults("sE")
  }
  private def findResultsForEntityForRun(runName: String, entity : String): (List[String], List[String], Int, Int, Boolean) = {
    val notRecalledQuery =
      s"""
         |select ?nR
         |where {
         |  <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> ?o .
         |  ?o <${ResultsSimilarArtistsGlobals.qEntity}> <$entity> .
         |  ?o <${ResultsSimilarArtistsGlobals.notRecalled}> ?nR .
         |}
       """.stripMargin
    val timeAndEntityCountQuery =
      s"""
         |select ?eT ?feT ?hT
         |where {
         |  <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> ?o .
         |  ?o <${ResultsSimilarArtistsGlobals.qEntity}> <$entity> .
         |  ?o <${ResultsSimilarArtistsGlobals.execTime}> ?eT .
         |  ?o <${ResultsSimilarArtistsGlobals.foundEntitiesCount}> ?feT .
         |  ?o <${ResultsSimilarArtistsGlobals.hadTimeout}> ?hT .
         |}
         """.stripMargin
    val recalled: List[String] = findRecalledEntities(runName, entity)
    val notRecalled : List[String] = executeQuery(notRecalledQuery).getResults("nR")
    val timeAndEntityCount =  executeQuery(timeAndEntityCountQuery)
    val execTime : Int = timeAndEntityCount.getResults("eT")(0)
    val foundEntitiesCount : Int = timeAndEntityCount.getResults("feT")(0)
    val hadTimeout : Boolean = timeAndEntityCount.getResults("hT")(0)
    return (recalled, notRecalled, execTime, foundEntitiesCount, hadTimeout)
  }
  def findFeatures(runName: String, qEntity: String, recalledEntity: String) : (List[String], List[Double]) = {
    val queryString =
      recalledQuery(runName, qEntity).init +
      s"""
         |?bR <${ResultsSimilarArtistsGlobals.featureFound}> ?fF .
         |?fF <${ResultsSimilarArtistsGlobals.featureName}> ?fN .
         |?fF <${ResultsSimilarArtistsGlobals.featureWeight}> ?fW .}""".stripMargin
    val correctSelect = queryString.replace("select ?r", "select ?fN ?fW")
    val correctSelectWithRecalled = correctSelect.replace("?r", s"<$recalledEntity>")
    val query = executeQuery(correctSelectWithRecalled)
    (query.getResults("fN"), query.getResults("fW"))

  }

  private def recalledQuery(runName: String, qEntity: String) : String = {
    val recalledQuery =
      s"""
         |select ?r
         |where {
         |  <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> ?o .
         |  ?o <${ResultsSimilarArtistsGlobals.qEntity}> <$qEntity> .
         |  ?o <${ResultsSimilarArtistsGlobals.recalled}> ?bR .
         |  ?bR <${ResultsSimilarArtistsGlobals.entityRelation}> ?r .
         |}""".stripMargin
    recalledQuery
  }

  def findRecalledEntities(runName: String, qEntity: String) : List[String]= {
    val recalled: List[String] = executeQuery(recalledQuery(runName, qEntity)).getResults("r")
    recalled
  }

  //Want Map[qEntity,(Recalled, notRecalled, time, foundEntitiesCount)
  def findResultsForRun(runName : String): List[(String, (List[String], List[String], Int, Int, Boolean))] = {
    val qEntities: List[String] = findQEntitiesForRun(runName)
    return qEntities.map(qEntity => (qEntity -> findResultsForEntityForRun(runName, qEntity)))
  }

  def findQEntitiesForRun(runName: String) = {
    val findAllQEntities =
      s"""
         |select ?qE
         |where {
         |  <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> ?o .
         |  ?o <${ResultsSimilarArtistsGlobals.qEntity}> ?qE .
         |}
       """.stripMargin
    val qEntities: List[String] = executeQuery(findAllQEntities).getResults("qE")
    qEntities
  }

  def findAllRuns(): List[String] = {
    val findRuns =
      s"""
         |select *
         |where {
         |  ?s <${SimilarPropertyOntology.rdfType}> <${ResultsSimilarArtistsGlobals.runType}> .
         |}
       """.stripMargin
    return executeQuery(findRuns).getResults("s")
  }
  def findStatementCountEntity(entity : String) : Int = {
    val findStatementCount =
      s"""
         |select ?o
         |where {
         |  <$entity> <${ResultsSimilarArtistsGlobals.statementCount}> ?o .
         |}
       """.stripMargin
    return executeQuery(findStatementCount).getResults("o")(0)
  }
  def findStatsForRun(runName : String) : (Double, Double, Int, Int, Int) = {
      val query =
        s"""
           |select * where { <${runName}> <${ResultsSimilarArtistsGlobals.recall}> ?r ;
           |                      <${ResultsSimilarArtistsGlobals.precision}> ?p ;
           |                      <${ResultsSimilarArtistsGlobals.avgExecTime}> ?eT ;
           |                      <${ResultsSimilarArtistsGlobals.avgFoundEntities}> ?afe ;
           |                      <${ResultsSimilarArtistsGlobals.percentTimeout}> ?pT .
           |}
      """.stripMargin
    val executed= executeQuery(query)
    return (executed.getResults("r")(0), executed.getResults("p")(0), executed.getResults("eT")(0), executed.getResults("afe")(0), executed.getResults("pT")(0))
    }
  def findQueryEntities(): List[String] = {
    val query =
      s"""
         |select ?qE
         |where {
         |  ?qE a <${ResultsSimilarArtistsGlobals.qEntity}>
         |}
       """.stripMargin
    return executeQuery(query).getResults("qE")
  }
  def findLabelForEntity(id : String) : Try[String] = {
    val kg = KnowledgeGraph.findKnowledgeGraphFromId(id)
    if(kg != KnowledgeGraph.wikidata) throw new NotImplementedError()
    val query =
      s"""
         |select ?l
         |where {
         |  <$id> <${SimilarPropertyOntology.rdfsLabel}> ?l .
         |}""".stripMargin
    Try(executeQuery(query).getResults("l")(0)) match {
      case Success(label) => {
        UpdateQueryFactory.addLabelForEntity(id, label)
        Try(label)
      }
      case Failure(_) => {
        val commonPrefixes = "PREFIX wd: <http://www.wikidata.org/entity/>\nPREFIX wdt: <http://www.wikidata.org/prop/direct/>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
        val labelQuery = new QueryForOnlineWikidata(() => commonPrefixes + s"select ?label \n where { <$id> rdfs:label ?label . \n filter(lang(?label) = 'en')\n }")
        labelQuery.executeRaw()
        Try(labelQuery.getResults("label")(0))
      }
    }
  }


  }
