package core.query.specific

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{KnowledgeGraphs, MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import core.query.variables.ResultVariable
import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 01.12.2016.
  */
object UpdateQueryFactory {

  val datatypeInteger = """^^<http://www.w3.org/2001/XMLSchema#integer>"""
  val datatypeDouble = """^^<http://www.w3.org/2001/XMLSchema#double>"""
  val datatypeBoolean = SimilarPropertyOntology.datatypeBoolean

  def updateData(insertQuery : String)(implicit knowledgeGraph: KnowledgeGraph): Unit = {
    QueryLocalServer.updateLocalData(insertQuery,DatasetInferrer.getDataset(insertQuery))
  }
  def updateData(insertQuery : String, datasetForced : String): Unit = {
    QueryLocalServer.updateLocalData(insertQuery, datasetForced)
  }
  def addPropertyDistribution(entityType: String, propertyDistribution: Map[String, (Double, Int, Double, Int)])(implicit knowledgeGraph: KnowledgeGraph) : Unit = {
    val insertQuery = propertyDistribution.map{
      case (property, (importanceRatioDomain, domainCount,importanceRatioRange, rangeCount)) =>
        val domainCountLine = if(domainCount > 0) s"""<${SimilarPropertyOntology.domainCount}> "$domainCount"$datatypeInteger ;""" else ""
        val rangeCountLine = if(rangeCount > 0) s"""<${SimilarPropertyOntology.rangeCount}> "$rangeCount"$datatypeInteger ;""" else ""
        val importanceRatioDomainLine = if(domainCount > 0) s"""<${SimilarPropertyOntology.typePropertyRatioDomain}> "$importanceRatioDomain"$datatypeDouble ;""" else ""
        val importanceRatioRangeLine = if(rangeCount > 0) s"""<${SimilarPropertyOntology.typePropertyRatioRange}> "$importanceRatioRange"$datatypeDouble ;""" else ""
        s"""
           |<$entityType> <${SimilarPropertyOntology.propertyDistributionNode}> [
           |  $domainCountLine
           |  $importanceRatioDomainLine
           |  $rangeCountLine
           |  $importanceRatioRangeLine
           |  <${SimilarPropertyOntology.distributionForProperty}> <$property>
           | ] .
         """.stripMargin
    }.mkString("\n")
    updateData(s"insert { $insertQuery } where {}")
  }

  def updateHierachyLevel(entityType: String, hierarchyLevel: Int)(implicit knowledgeGraph: KnowledgeGraph) : Unit = {
    val deleteQuery = s"""<$entityType> <${SimilarPropertyOntology.hierarchyLevel}> ?o"""
    val insertQuery =
      s"""
insert { <$entityType> <${SimilarPropertyOntology.hierarchyLevel}> "$hierarchyLevel"$datatypeInteger } where {}
       """.stripMargin
    cleanDatasetWhere(DatasetInferrer.getDataset(deleteQuery), deleteQuery)
    updateData(insertQuery)

  }
  def addLabelForEntity(enitity: String, label: String) : Unit = {
    val insertQuery =
      s"""
         |insert { <$enitity> <${SimilarPropertyOntology.rdfsLabel}> "$label"@en } where {}
       """.stripMargin
   updateData(insertQuery, MyDatasets.resultsSimilarArtists)
  }
  def addStatsForRun(stats: (String, Double, Double, Int, Int, Int)): Unit = {
    val insertQuery =
      s"""
       |insert { <${stats._1}> <${ResultsSimilarArtistsGlobals.recall}> "${stats._2}"$datatypeDouble;
       |                      <${ResultsSimilarArtistsGlobals.precision}> "${stats._3}"$datatypeDouble;
       |                      <${ResultsSimilarArtistsGlobals.avgExecTime}> "${stats._4}"$datatypeInteger;
       |                      <${ResultsSimilarArtistsGlobals.avgFoundEntities}> "${stats._5}"$datatypeInteger;
       |                      <${ResultsSimilarArtistsGlobals.percentTimeout}> "${stats._6}"$datatypeInteger;
       |                        } where {}
      """.stripMargin
   updateData(insertQuery, MyDatasets.resultsSimilarArtists)
  }
  def addExpectedSimilarsForQEntity(qEntity: String, expectedSimilars : List[String]) = {
    val expectedSimilarsStatements = expectedSimilars.map(s => s"<$qEntity> <${ResultsSimilarArtistsGlobals.expectedSimilar}> <$s>").mkString(".\n")
    val queryString =
      s"""
         |insert {
         |<$qEntity> a <${ResultsSimilarArtistsGlobals.qEntity}> .
         |$expectedSimilarsStatements
         |                        } where {}
      """.stripMargin
    QueryLocalServer.updateLocalData(queryString, MyDatasets.resultsSimilarArtists)
  }
  def updateValueCount(propertyAsFullString: String, entity: String, count: Int)(implicit knowledgeGraph: KnowledgeGraph) : Unit= {
    val prefix = KnowledgeGraphs.getDatasetEntityPrefix(knowledgeGraph)
    if(!entity.startsWith(prefix)) return
    val insertQuery = s"insert { <$propertyAsFullString> <${SimilarPropertyOntology.valueMatchProperty}> [ <${SimilarPropertyOntology.valueMatchValue}> <$entity>;\n" +
      s"""<${SimilarPropertyOntology.valueMatchCount}> "%d" ] } where {}""".format(count)
    val dataset = DatasetInferrer.getDataset(insertQuery)
    println(s"Adding to $dataset insertQuery: $insertQuery")
   updateData(insertQuery)
  }
  def addResult(qEntity: String, foundEntity : String, ranking : Int, simScore: Double) = {
    val insertQuery = addResultQuery(qEntity, foundEntity, ranking, simScore)
   updateData(insertQuery, MyDatasets.resultsSimilarArtists)
  }

  def addResultQuery(qEntity: String, foundEntity: String, ranking: Int, simScore: Double): String = {
    return s"""insert { <$qEntity> <${ResultsSimilarArtistsGlobals.similar}> [ <${ResultsSimilarArtistsGlobals.ranking}> "$ranking";\n""" +
      s"""<${ResultsSimilarArtistsGlobals.foundEntity}> <${foundEntity}> ; \n <${ResultsSimilarArtistsGlobals.simScore}> "${simScore}" ] } where {}"""
  }
  def addNewRun(runName: String, cleanFirst : Boolean = true): Unit ={
    if(cleanFirst) cleanRunName(runName)
    val insertQuery = s"""insert { <$runName> <${SimilarPropertyOntology.rdfType}> <${ResultsSimilarArtistsGlobals.runType}>} where {}"""
   updateData(insertQuery, MyDatasets.resultsSimilarArtists)
  }

  def cleanRunName(runName: String) = {
    cleanDatasetWhere(MyDatasets.resultsSimilarArtists, s"<$runName> ?p ?o")
  }

  def addFindSimilarResult(runName: String, qEntity: String, recalled: List[String], notRecalled : List[String], execTime2: Int, foundEntitiesCount: Int, hadTimeout: Boolean): Unit ={
    val insertQuery = addFindSimilarResultQuery(runName, qEntity, recalled, notRecalled, execTime2, foundEntitiesCount, hadTimeout)
   updateData(insertQuery, MyDatasets.resultsSimilarArtists)
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


  def cleanDataset(dataset : String): Unit = {
    QueryLocalServer.deleteLocalData(dataset)
  }
  def cleanDatasetWhere(dataset: String, query: String) : Unit = {
    QueryLocalServer.deleteLocalData(dataset,query)
  }
  def addStatementCount(entity: String, count : Int) = {
    val insertQuery = s"""insert { <${entity}> <${ResultsSimilarArtistsGlobals.statementCount}> "${count}" } where {} """
   updateData(insertQuery, MyDatasets.resultsSimilarArtists)
  }
  def addStatements(statements: Iterable[String], dataset: String = MyDatasets.DsBig) = {
    val insertQuery = "insert { \n %s } \n where {}".format(statements.mkString("\n"))
   updateData(insertQuery, dataset)
  }
  def addIsDescriptive(property : String, isDescriptive: Boolean)(implicit knowledgeGraph: KnowledgeGraph) = {
    val insertQuery =
      s"""
       |insert { <$property> <${SimilarPropertyOntology.isDescriptive}> "$isDescriptive"^^<$datatypeBoolean>} where {}

    """.stripMargin
   updateData(insertQuery)
  }
  def addDomainAndRangeTypesForProperty(domains: List[String], ranges: List[String], property: String)(implicit knowledgeGraph: KnowledgeGraph): Unit = {
    val domainStatements = domains.map(d => s"<$d> <${SimilarPropertyOntology.isDomainType}> <$property> .").mkString("\n")
    val rangeStatements = ranges.map(r => s"<$r> <${SimilarPropertyOntology.isRangeType}> <$property> .").mkString("\n")
    val insertQuery =
      s"""
         |insert { $domainStatements $rangeStatements} where {}

    """.stripMargin
   updateData(insertQuery)
  }

}
