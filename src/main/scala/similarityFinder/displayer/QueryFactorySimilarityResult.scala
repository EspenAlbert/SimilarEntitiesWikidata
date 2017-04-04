package similarityFinder.displayer

import core.globals.{MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import core.query.Query
import core.query.specific.UpdateQueryFactory.{datatypeDouble, datatypeInteger}
import core.query.variables.ResultVariable

/**
  * Created by espen on 30.03.17.
  */
object QueryFactorySimilarityResult {
  private def executeQuery(queryString: String): Query = {
    val query = new Query(() => queryString, MyDatasets.resultsSimilarArtists)
    query.execute()
    query
  }
  private def findResultsForEntityForRun(runName: String, entity : String): (List[String], List[String], Int, Int, Boolean) = {
    val recalledQuery =
      s"""
         |select ?r
         |where {
         |  <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> ?o .
         |  ?o <${ResultsSimilarArtistsGlobals.qEntity}> <$entity> .
         |  ?o <${ResultsSimilarArtistsGlobals.recalled}> ?r .
         |}
       """.stripMargin
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
    val recalled : List[String] = executeQuery(recalledQuery).getResults("r")
    val notRecalled : List[String] = executeQuery(notRecalledQuery).getResults("nR")
    val timeAndEntityCount =  executeQuery(timeAndEntityCountQuery)
    val execTime : Int = timeAndEntityCount.getResults("eT")(0)
    val foundEntitiesCount : Int = timeAndEntityCount.getResults("feT")(0)
    val hadTimeout : Boolean = timeAndEntityCount.getResults("hT")(0)
    return (recalled, notRecalled, execTime, foundEntitiesCount, hadTimeout)
  }

  //Want Map[qEntity,(Recalled, notRecalled, time, foundEntitiesCount)
  def findResultsForRun(runName : String): List[(String, (List[String], List[String], Int, Int, Boolean))] = {
    val findAllQEntities =
      s"""
         |select ?qE
         |where {
         |  <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> ?o .
         |  ?o <${ResultsSimilarArtistsGlobals.qEntity}> ?qE .
         |}
       """.stripMargin
    val qEntities : List[String] = executeQuery(findAllQEntities).getResults("qE")
    return qEntities.map(qEntity => (qEntity -> findResultsForEntityForRun(runName, qEntity)))
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


  }
