package query.specific

import globals.{MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import jenaQuerier.QueryLocalServer
import rdf.SimpleRDF

/**
  * Created by Espen on 01.12.2016.
  */
object UpdateQueryFactory {
  def addToLocalDataset(statements: SimpleRDF*): String = {
    val statementsInsert = statements.map(_.getStatementNt()).mkString("\n")
    val insertLine = "insert { \n %s } \n where {}".format(statementsInsert)
    try {
      QueryLocalServer.updateLocalData(insertLine)
    } catch{
      case e => println("Failed to update: " + insertLine)
    }
    insertLine
  }

  def updateValueCount(propertyAsFullString: String, entity: String, count: Int) = {
    val updateQuery = s"insert { <$propertyAsFullString> <${SimilarPropertyOntology.valueMatchProperty}> [ <${SimilarPropertyOntology.valueMatchValue}> <$entity>;\n" +
      s"""<${SimilarPropertyOntology.valueMatchCount}> "%d" ] } where {}""".format(count)
    println(updateQuery)
    QueryLocalServer.updateLocalData(updateQuery)
  }
  def addResult(qEntity: String, foundEntity : String, ranking : Int, simScore: Double) = {
    QueryLocalServer.updateLocalData(addResultQuery(qEntity, foundEntity, ranking, simScore), MyDatasets.ResultsSimilarArtists)
  }

  def addResultQuery(qEntity: String, foundEntity: String, ranking: Int, simScore: Double): String = {
    return s"""insert { <$qEntity> <${ResultsSimilarArtistsGlobals.similar}> [ <${ResultsSimilarArtistsGlobals.ranking}> "$ranking";\n""" +
      s"""<${ResultsSimilarArtistsGlobals.foundEntity}> <${foundEntity}> ; \n <${ResultsSimilarArtistsGlobals.simScore}> "${simScore}" ] } where {}"""
  }
  def cleanDataset(dataset : String): Unit = {
    QueryLocalServer.deleteLocalData(dataset)
  }
  def addStatementCount(entity: String, count : Int) = {
    QueryLocalServer.updateLocalData(s"""insert { <${entity}> <${ResultsSimilarArtistsGlobals.statementCount}> "${count}" } where {} """, MyDatasets.ResultsSimilarArtists)
  }
}
