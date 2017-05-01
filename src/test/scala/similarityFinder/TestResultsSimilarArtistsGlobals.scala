package similarityFinder

import core.globals.{KnowledgeGraph, MyDatasets, ResultsSimilarArtistsGlobals}
import core.query.Query
import core.query.specific.UpdateQueryFactory
import core.strategies.PropertyMatchStrategy
import data.WikidataFactory
import iAndO.dump.DumpObject
import org.scalatest.FunSuite
import similarityFinder.displayer.{QueryFactorySimilarityResult, ResultHandler}
import tags.{ActiveSlowTag, ActiveTag}

/**
  * Created by espen on 06.02.17.
  */
class TestResultsSimilarArtistsGlobals extends FunSuite{
  test("should be able to print the different enumerative values"){
    for(v <- ResultsSimilarArtistsGlobals.values) {
      println(v)
    }
  }
  test("A proper query should be generated, elton john similar to rod stewart") {
    println(UpdateQueryFactory.addResultQuery("http://www.wikidata.org/entity/Q2808","http://www.wikidata.org/entity/Q182655",50, 968.81))
  }
  test("Add data to the db should work") {
    UpdateQueryFactory.addResult("http://www.wikidata.org/entity/Q2808","http://www.wikidata.org/entity/Q182655",50, 968.81)
    val queryString: String = queryForFindingQEntitiesAndTheirSimilars
    val query = new Query(() => queryString, MyDatasets.resultsSimilarArtists)
    query.execute()
    query.getResults("o").foreach(s => println(s.value))
  }

  private def queryForFindingQEntitiesAndTheirSimilars = {
    val queryString =
      s"""select ?s ?o where {
         ?s <http://www.espenalbert.com/rdf/resultsSimilarArtists#similar> ?blank .
         ?blank <http://www.espenalbert.com/rdf/resultsSimilarArtists#foundEntity> ?o . }
       """
    queryString
  }
  private def queryForFindingStatementCount(entity : String) : String = {
    return s"""select ?c where {
        |<${entity}> <${ResultsSimilarArtistsGlobals.statementCount}> ?c }
      """.stripMargin
  }
  test("Adding a statement count should work") {
    val eltonJohn = "http://www.wikidata.org/entity/Q2808"
    val count = 500
    UpdateQueryFactory.addStatementCount(eltonJohn, count)
    val queryString: String = queryForFindingStatementCount(eltonJohn)
    val query = new Query(() => queryString, MyDatasets.resultsSimilarArtists)
    query.execute()
    assert(count == query.getResults("c")(0))

  }

  test("cleaning the db should work") {
    UpdateQueryFactory.cleanDataset(MyDatasets.resultsSimilarArtists)
    val queryString: String = queryForFindingQEntitiesAndTheirSimilars
    val query = new Query(() => queryString, MyDatasets.resultsSimilarArtists)
    query.execute()
    assert(query.getResults("o").length == 0)
  }
  val wd = WikidataFactory
  val starr = wd.ringoStarr
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val runName = RunName.getRunName(List(PropertyMatchStrategy.name)) + "Test"
  test("Should be able to add a similarResult", ActiveTag) {
    val start = System.currentTimeMillis()
    val qEntity = starr.id
    val recalled = List(wd.paulMcCartney, wd.johnLennon)
    val sutcliffe = wd.stuartSutcliffe
    val notRecalled = List(sutcliffe)
    val foundEntities = 1000
    val hadTimeout1 = false
    val execTime = System.currentTimeMillis() - start
    UpdateQueryFactory.cleanDatasetWhere(MyDatasets.resultsSimilarArtists, s"<$runName> ?p ?o")
    UpdateQueryFactory.addFindSimilarResult(runName,qEntity, recalled, notRecalled, execTime.toInt, foundEntities, hadTimeout1)
    val actualFromDB = QueryFactorySimilarityResult.findResultsForRun(runName).head
    actualFromDB match {
      case (entity, (lRecalled, lNotRecalled, eTime, fECount, hadTimeout)) => {
        assert(entity == qEntity)
        assert(lRecalled == recalled)
        assert(notRecalled == lNotRecalled)
        assert(execTime == eTime)
        assert(foundEntities == fECount)
        assert(hadTimeout1 == hadTimeout)
      }}
  }
  test("Adding a new run should work", ActiveTag) {
    UpdateQueryFactory.addNewRun(runName)
    val runs = QueryFactorySimilarityResult.findAllRuns()
    assert(runs.contains(runName))
  }
  test("Calculating recalls should work", ActiveSlowTag) {
    ResultHandler.calculateRecall()
  }
  test("Calculating stats for a specific run should work", ActiveTag) {
    val runName = "http://www.espenalbert.com/rdf/resultsSimilarArtists#wikidata-SearchUndirectedL2Strategy--SampleRun"
    val timeouts = 5
    val queriesCount = 10
    val percentTimedOut = BigDecimal((timeouts.toDouble / queriesCount)*100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toInt
    assert(percentTimedOut==50)
    ResultHandler.calculateRecall(runName)
    QueryFactorySimilarityResult.findStatsForRun(runName) match {
      case (r, p, execTime, statementCount, percentTimeout) => {
        assert(r > 0.1)
        assert(p < 0.9)
        assert(execTime > 1000)
        assert(statementCount > 1000)
        assert(percentTimeout > 90)
      }
    }
  }
  test("Finding not found pairs should work") {
    val baselineRun = "http://www.espenalbert.com/rdf/resultsSimilarArtists#wikidata-SearchUndirectedL2Strategy-"
    val notFoundPairs = for{
      (qEntity, (_, notRecalled, _, _, _)) <- QueryFactorySimilarityResult.findResultsForRun(baselineRun)
      notRecalledEntity <- notRecalled
    } yield(qEntity, notRecalledEntity)
//    notFoundPairs.foreach(pair =>
//    println(s"Not found: ${pair._1} - ${pair._2} Statement count: ${QueryFactorySimilarityResult.findStatementCountEntity(pair._1)}-${QueryFactorySimilarityResult.findStatementCountEntity(pair._2)}"))
    DumpObject.dumpJsonMapStringListString(notFoundPairs.groupBy(_._1).map{
      case (qEntity, listOfPairs) => qEntity -> listOfPairs.map(_._2)
    }, "wikidata-qEntityToNotRecalled")
  }

}
