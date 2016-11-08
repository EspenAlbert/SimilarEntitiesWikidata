package query

import globals.MyDatasets
import org.scalatest.FunSuite
import query.specific.QueryFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindIDForPropertyLabelQuery extends FunSuite{
  test("Should be exactly 199 entities of country") {
    QueryFactory.dataset = MyDatasets.SimilarProperties
    val countId: String = QueryFactory.findIDForPropertyLabelQuery("count")
    assert(countId == "<http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#OWLDataProperty_16dc36fd_ea2f_4c8d_9c52_9b10d249a067>")
    print(countId)
  }

}
