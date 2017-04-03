package similarityFinder

import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import data.WikidataFactory
import org.scalatest.FunSuite
import similarityFinder.displayer.QueryFactorySimilarityResult

/**
  * Created by espen on 03.04.17.
  */
class TestStatementCounter extends FunSuite{
  test("SHould work for wikidata entities"){
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    StatementCounter.countEntityStatements
  }
  test("Sample test ringo starr") {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val ringoStarrWD = WikidataFactory.ringoStarr.id
    val prefixWD = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    val testIds = List("Q498715",  "Q727090","Q498715").map(prefixWD + _) ++ List(ringoStarrWD)
    testIds.foreach(e => {
      val actual = QueryFactorySimilarityResult.findStatementCountEntity(e)
      val expected = new GraphRDF(e).getStatementCountWithoutTypeStatements
      assert(actual == expected)
    })
  }
  test("SHould work for dbpedia entities"){
    implicit val knowledgeGraph = KnowledgeGraph.dbPedia
    StatementCounter.countEntityStatements
  }

}
