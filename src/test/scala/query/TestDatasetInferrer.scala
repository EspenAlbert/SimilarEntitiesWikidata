package query

import core.globals.{KnowledgeGraphs, MyDatasets, ResultsSimilarArtistsGlobals, SimilarPropertyOntology}
import core.query.specific.DatasetInferrer
import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestDatasetInferrer extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraphs.wikidata
  test("The wikidata dataset should be inferred properly") {
    val query =
      """
        |PREFIX wd: <http://www.wikidata.org/entity/>
        |select *
        |where {
        | wd:Q76 ?p wd:Q13133
        | }
      """.stripMargin
    assert(DatasetInferrer.getDataset(query) == MyDatasets.DsBig)
  }
  test("The similar property dataset should be inferred properly") {
    val query: String = getQueryForProperty(SimilarPropertyOntology.domainCount)
    assert(DatasetInferrer.getDataset(query) == MyDatasets.strategyMappingWikidata)
    assert(DatasetInferrer.getDataset("SELECT  ?hL WHERE   { <http://www.wikidata.org/entity/Q35120>               <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#hierarchyLevel>  ?hL   } ") == MyDatasets.strategyMappingWikidata)
  }


  private def getQueryForProperty(property : String) : String = {
    val query =
      s"""
         |PREFIX wd: <http://www.wikidata.org/entity/>
         |select *
         |where {
         | wd:Q76 <$property> wd:QXX
         | }

    """.
        stripMargin
    query
  }

  test("The value match dataset should be inferred properly") {
    assert(DatasetInferrer.getDataset(getQueryForProperty(SimilarPropertyOntology.valueMatchClass)) == MyDatasets.valueMatchWikidata)
    assert(DatasetInferrer.getDataset(getQueryForProperty(SimilarPropertyOntology.valueMatchValue)) == MyDatasets.valueMatchWikidata)
    assert(DatasetInferrer.getDataset(getQueryForProperty(SimilarPropertyOntology.valueMatchCount)) == MyDatasets.valueMatchWikidata)
  }



}
