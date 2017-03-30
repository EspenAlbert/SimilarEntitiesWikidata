package core.query.specific

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 01.12.2016.
  */
object UpdateQueryFactory {

  def updateValueCount(propertyAsFullString: String, entity: String, count: Int)(implicit knowledgeGraph: KnowledgeGraph) = {
    val updateQuery = s"insert { <$propertyAsFullString> <${SimilarPropertyOntology.valueMatchProperty}> [ <${SimilarPropertyOntology.valueMatchValue}> <$entity>;\n" +
      s"""<${SimilarPropertyOntology.valueMatchCount}> "%d" ] } where {}""".format(count)
    val dataset = DatasetInferrer.getDataset(updateQuery)
    println(s"Adding to $dataset updateQuery: $updateQuery")
    QueryLocalServer.updateLocalData(updateQuery, dataset)
  }
  def addResult(qEntity: String, foundEntity : String, ranking : Int, simScore: Double) = {
    QueryLocalServer.updateLocalData(addResultQuery(qEntity, foundEntity, ranking, simScore), MyDatasets.resultsSimilarArtists)
  }

  def addResultQuery(qEntity: String, foundEntity: String, ranking: Int, simScore: Double): String = {
    return s"""insert { <$qEntity> <${ResultsSimilarArtistsGlobals.similar}> [ <${ResultsSimilarArtistsGlobals.ranking}> "$ranking";\n""" +
      s"""<${ResultsSimilarArtistsGlobals.foundEntity}> <${foundEntity}> ; \n <${ResultsSimilarArtistsGlobals.simScore}> "${simScore}" ] } where {}"""
  }
  def addNewRun(runName: String): Unit ={
    val insertQuery = s"""insert { <$runName> <${SimilarPropertyOntology.rdfType}> <${ResultsSimilarArtistsGlobals.runType}>} where {}"""
    QueryLocalServer.updateLocalData(insertQuery, MyDatasets.resultsSimilarArtists)
  }
  def addFindSimilarResult(runName: String, qEntity: String, recalled: List[String], notRecalled : List[String], execTime2: Int, foundEntitiesCount: Int): Unit ={
    QueryLocalServer.updateLocalData(addFindSimilarResultQuery(runName, qEntity, recalled, notRecalled, execTime2, foundEntitiesCount), MyDatasets.resultsSimilarArtists)
  }
  def addFindSimilarResultQuery(runName: String, qEntity: String, recalled: List[String], notRecalled : List[String], execTime2: Int, foundEntitiesCount: Int) : String = {
    val recalledInsert = recalled.map(s=> s"<${ResultsSimilarArtistsGlobals.recalled}> <$s>;").mkString("\n")
    val notRecalledInsert = notRecalled.map(s=> s"<${ResultsSimilarArtistsGlobals.notRecalled}> <$s>;").mkString("\n")
    return s"""
              |insert { <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> [
              | <${ResultsSimilarArtistsGlobals.qEntity}> <$qEntity>;
              | <${ResultsSimilarArtistsGlobals.execTime}> "$execTime2"^^<http://www.w3.org/2001/XMLSchema#integer> ;
              | <${ResultsSimilarArtistsGlobals.foundEntitiesCount}> "${foundEntitiesCount}";
              |  ${recalledInsert}
              |  ${notRecalledInsert}
              | ] } where {}
              |""".stripMargin
  }
  def cleanDataset(dataset : String): Unit = {
    QueryLocalServer.deleteLocalData(dataset)
  }
  def cleanDatasetWhere(dataset: String, query: String) : Unit = {
    QueryLocalServer.deleteLocalData(dataset,query)
  }
  def addStatementCount(entity: String, count : Int) = {
    QueryLocalServer.updateLocalData(s"""insert { <${entity}> <${ResultsSimilarArtistsGlobals.statementCount}> "${count}" } where {} """, MyDatasets.resultsSimilarArtists)
  }
  def addStatements(statements: Iterable[String], dataset: String = MyDatasets.DsBig) = {
    QueryLocalServer.updateLocalData("insert { \n %s } \n where {}".format(statements.mkString("\n")), dataset)
  }
}
