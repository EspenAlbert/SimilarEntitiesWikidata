package similarityFinder.displayer

import core.globals.{MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import core.query.Query
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
  private def findResultsForEntityForRun(runName: String, entity : String): (List[String], List[String], Int, Int) = {
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
         |select ?eT ?feT
         |where {
         |  <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> ?o .
         |  ?o <${ResultsSimilarArtistsGlobals.qEntity}> <$entity> .
         |  ?o <${ResultsSimilarArtistsGlobals.execTime}> ?eT .
         |  ?o <${ResultsSimilarArtistsGlobals.foundEntitiesCount}> ?feT .
         |}
         """.stripMargin
    val recalled : List[String] = executeQuery(recalledQuery).getResults("r")
    val notRecalled : List[String] = executeQuery(notRecalledQuery).getResults("nR")
    val timeAndEntityCount =  executeQuery(timeAndEntityCountQuery)
    val execTime : Int = timeAndEntityCount.getResults("eT")(0)
    val foundEntitiesCount : Int = timeAndEntityCount.getResults("feT")(0)
    return (recalled, notRecalled, execTime, foundEntitiesCount)
  }

  //Want Map[qEntity,(Recalled, notRecalled, time, foundEntitiesCount)
  def findResultsForRun(runName : String): List[(String, (List[String], List[String], Int, Int))] = {
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

}
