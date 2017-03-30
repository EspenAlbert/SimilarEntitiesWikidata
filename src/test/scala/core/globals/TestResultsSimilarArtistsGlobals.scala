package core.globals

import core.query.Query
import core.query.specific.UpdateQueryFactory
import core.strategies.PropertyMatchStrategy
import data.WikidataFactory
import org.scalatest.FunSuite
import similarityFinder.RunName
import similarityFinder.displayer.{QueryFactorySimilarityResult, ResultHandler}
import tags.ActiveTag

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
    assert(count == query.getResults("c")(0).toInt)

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
  val runName = RunName.getRunName(List(starr.propertyMatchStrategyLifeStyle)) + "Test"
  test("Should be able to add a similarResult", ActiveTag) {
    val start = System.currentTimeMillis()
    val qEntity = starr.id
    val recalled = List(wd.paulMcCartney, wd.johnLennon)
    val sutcliffe = wd.stuartSutcliffe
    val notRecalled = List(sutcliffe)
    val foundEntities = 1000
    val execTime = System.currentTimeMillis() - start
    UpdateQueryFactory.cleanDatasetWhere(MyDatasets.resultsSimilarArtists, s"<$runName> ?p ?o")
    UpdateQueryFactory.addFindSimilarResult(runName,qEntity, recalled, notRecalled, execTime.toInt, foundEntities)
    val actualFromDB = QueryFactorySimilarityResult.findResultsForRun(runName).head
    actualFromDB match {
      case (entity, (lRecalled, lNotRecalled, eTime, fECount)) => {
        assert(entity == qEntity)
        assert(lRecalled == recalled)
        assert(notRecalled == lNotRecalled)
        assert(execTime == eTime)
        assert(foundEntities == fECount)
      }}
  }
  test("Adding a new run should work", ActiveTag) {
    UpdateQueryFactory.addNewRun(runName)
    val runs = QueryFactorySimilarityResult.findAllRuns()
    assert(runs.contains(runName))
  }
  test("Calculating recalls should work", ActiveTag) {
    ResultHandler.calculateRecall
  }

}
