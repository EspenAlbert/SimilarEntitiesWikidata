package core.globals

import core.query.Query
import core.query.specific.UpdateQueryFactory
import org.scalatest.FunSuite

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
    val query = new Query(() => queryString, MyDatasets.ResultsSimilarArtists)
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
    val query = new Query(() => queryString, MyDatasets.ResultsSimilarArtists)
    query.execute()
    assert(count == query.getResults("c")(0).toInt)

  }

  test("cleaning the db should work") {
    UpdateQueryFactory.cleanDataset(MyDatasets.ResultsSimilarArtists)
    val queryString: String = queryForFindingQEntitiesAndTheirSimilars
    val query = new Query(() => queryString, MyDatasets.ResultsSimilarArtists)
    query.execute()
    assert(query.getResults("o").length == 0)
  }
}
