package core.query.specific

import core.feature.Feature
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{KnowledgeGraph, MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 01.12.2016.
  */
object UpdateQueryFactory {

  val datatypeInteger = """^^<http://www.w3.org/2001/XMLSchema#integer>"""
  val datatypeDouble = """^^<http://www.w3.org/2001/XMLSchema#double>"""
  val datatypeBoolean = SimilarPropertyOntology.datatypeBoolean
  def addStatsForRun(stats: (String, Double, Double, Int, Int, Int)): Unit = {
    val query =
      s"""
       |insert { <${stats._1}> <${ResultsSimilarArtistsGlobals.recall}> "${stats._2}"$datatypeDouble;
       |                      <${ResultsSimilarArtistsGlobals.precision}> "${stats._3}"$datatypeDouble;
       |                      <${ResultsSimilarArtistsGlobals.avgExecTime}> "${stats._4}"$datatypeInteger;
       |                      <${ResultsSimilarArtistsGlobals.avgFoundEntities}> "${stats._5}"$datatypeInteger;
       |                      <${ResultsSimilarArtistsGlobals.percentTimeout}> "${stats._6}"$datatypeInteger;
       |                        } where {}
      """.stripMargin
    QueryLocalServer.updateLocalData(query, MyDatasets.resultsSimilarArtists)
  }
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
  def addNewRun(runName: String, cleanFirst : Boolean = true): Unit ={
    if(cleanFirst) cleanRunName(runName)
    val insertQuery = s"""insert { <$runName> <${SimilarPropertyOntology.rdfType}> <${ResultsSimilarArtistsGlobals.runType}>} where {}"""
    QueryLocalServer.updateLocalData(insertQuery, MyDatasets.resultsSimilarArtists)
  }

  def cleanRunName(runName: String) = {
    cleanDatasetWhere(MyDatasets.resultsSimilarArtists, s"<$runName> ?p ?o")
  }

  def addFindSimilarResult(runName: String, qEntity: String, recalled: List[String], notRecalled : List[String], execTime2: Int, foundEntitiesCount: Int, hadTimeout: Boolean): Unit ={
    QueryLocalServer.updateLocalData(addFindSimilarResultQuery(runName, qEntity, recalled, notRecalled, execTime2, foundEntitiesCount, hadTimeout), MyDatasets.resultsSimilarArtists)
  }
  def addFindSimilarResultQuery(runName: String, qEntity: String, recalled: List[String], notRecalled : List[String], execTime2: Int, foundEntitiesCount: Int, hadTimeout: Boolean) : String = {
    val recalledInsert = recalled.map(s=> s"<${ResultsSimilarArtistsGlobals.recalled}> <$s>;").mkString("\n")
    val notRecalledInsert = notRecalled.map(s=> s"<${ResultsSimilarArtistsGlobals.notRecalled}> <$s>;").mkString("\n")
    return s"""
              |insert { <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> [
              | <${ResultsSimilarArtistsGlobals.qEntity}> <$qEntity>;
              | <${ResultsSimilarArtistsGlobals.execTime}> "$execTime2"$datatypeInteger ;
              | <${ResultsSimilarArtistsGlobals.foundEntitiesCount}> "${foundEntitiesCount}";
              | <${ResultsSimilarArtistsGlobals.hadTimeout}> "${hadTimeout}"^^<http://www.w3.org/2001/XMLSchema#boolean> ;
              |  ${recalledInsert}
              |  ${notRecalledInsert}
              | ] } where {}
              |""".stripMargin
  }
  def addFindSimilarResultWithFeatures(runName: String, qEntity: String, recalled: Map[String, List[Feature]], notRecalled : List[String], execTime2: Int, foundEntitiesCount: Int, hadTimeout: Boolean): Unit ={
    val query = addFindSimilarResultWithFeatureQuery(runName, qEntity, recalled, notRecalled, execTime2, foundEntitiesCount, hadTimeout)
    QueryLocalServer.updateLocalData(query, MyDatasets.resultsSimilarArtists)
  }
  def addFindSimilarResultWithFeatureQuery(runName: String, qEntity: String, recalled: Map[String, List[Feature]], notRecalled : List[String], execTime2: Int, foundEntitiesCount: Int, hadTimeout: Boolean) : String = {
    val recalledInsert = recalled.map(s=>{
      val featureLines = s._2.map(f =>
        saveFeatureStatements(f))
        .mkString(s" <${ResultsSimilarArtistsGlobals.featureFound}> ")
      s"""
         |<${ResultsSimilarArtistsGlobals.recalled}> [
         |<${ResultsSimilarArtistsGlobals.entityRelation}> <${s._1}>;
         |<${ResultsSimilarArtistsGlobals.featureFound}> ${featureLines}
         |];
       """.stripMargin}).mkString("\n")
    val notRecalledInsert = notRecalled.map(s=> s"<${ResultsSimilarArtistsGlobals.notRecalled}> <$s>;").mkString("\n")
    return s"""
              |insert { <$runName> <${ResultsSimilarArtistsGlobals.qEntityResult}> [
              | <${ResultsSimilarArtistsGlobals.qEntity}> <$qEntity>;
              | <${ResultsSimilarArtistsGlobals.execTime}> "$execTime2"$datatypeInteger ;
              | <${ResultsSimilarArtistsGlobals.foundEntitiesCount}> "${foundEntitiesCount}";
              | <${ResultsSimilarArtistsGlobals.hadTimeout}> "${hadTimeout}"^^<http://www.w3.org/2001/XMLSchema#boolean> ;
              |${recalledInsert}
              |${notRecalledInsert}
              | ] } where {}
              |""".stripMargin
  }

  private def saveFeatureStatements(f: Feature) = {
    s"""
       |[
       |<${ResultsSimilarArtistsGlobals.featureName}> "${f.toString}" ;
       |<${ResultsSimilarArtistsGlobals.featureWeight}> "${f.getScore()}"$datatypeDouble
       |];""".stripMargin
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
  def addIsDescriptive(property : String, isDescriptive: Boolean)(implicit knowledgeGraph: KnowledgeGraph) = {
    val dataset = KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph)
    val query =
      s"""
       |insert { <$property> <${SimilarPropertyOntology.isDescriptive}> "$isDescriptive"^^<$datatypeBoolean>} where {}

    """.stripMargin
    QueryLocalServer.updateLocalData(query,dataset)
  }
  def addDomainAndRangeTypesForProperty(domains: List[String], ranges: List[String], property: String)(implicit knowledgeGraph: KnowledgeGraph): Unit = {
    val dataset = KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph)
    val domainStatements = domains.map(d => s"<$d> <${SimilarPropertyOntology.isDomainType}> <$property> .").mkString("\n")
    val rangeStatements = ranges.map(r => s"<$r> <${SimilarPropertyOntology.isRangeType}> <$property> .").mkString("\n")
    val query =
      s"""
         |insert { $domainStatements $rangeStatements} where {}

    """.stripMargin
    QueryLocalServer.updateLocalData(query,dataset)
  }
}
