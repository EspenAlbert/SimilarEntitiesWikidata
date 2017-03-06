package query

import globals.{MyDatasets, SimilarPropertyOntology}
import org.scalatest.FunSuite
import query.specific.DatasetInferrer
import rdf.SimpleRDFFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestDatasetInferrer extends FunSuite{
  test("The wikidata dataset should be inferred properly") {
    val statment = SimpleRDFFactory.getStatement(("w:Q76", "w:P51", "w:QXX"))
    val query =
      """
        |PREFIX wd: <http://www.wikidata.org/entity/>
        |select *
        |where {
        | wd:Q76 ?p wd:Q13133
        | }
      """.stripMargin
    assert(DatasetInferrer.getDataset(statment.wherePhrase()) == MyDatasets.DsBig)
    assert(DatasetInferrer.getDataset(query) == MyDatasets.DsBig)
  }
  test("The similar property dataset should be inferred properly") {
    val statment = SimpleRDFFactory.getStatement(("w:Q76", SimilarPropertyOntology.domainCount.toString, "w:QXX"))
    assert(DatasetInferrer.getDataset(statment.wherePhrase()) == MyDatasets.SimilarProperties)
  }
  test("The value match dataset should be inferred properly") {
    val statment = SimpleRDFFactory.getStatement(("w:Q76", SimilarPropertyOntology.valueMatchClass.toString, "w:QXX"))
    assert(DatasetInferrer.getDataset(statment.wherePhrase()) == MyDatasets.ValueMatch)
    val statment2 = SimpleRDFFactory.getStatement(("w:Q76", SimilarPropertyOntology.valueMatchValue.toString, "w:QXX"))
    assert(DatasetInferrer.getDataset(statment2.wherePhrase()) == MyDatasets.ValueMatch)
    val statment3 = SimpleRDFFactory.getStatement(("w:Q76", SimilarPropertyOntology.valueMatchCount.toString, "w:QXX"))
    assert(DatasetInferrer.getDataset(statment3.wherePhrase()) == MyDatasets.ValueMatch)
  }


}
