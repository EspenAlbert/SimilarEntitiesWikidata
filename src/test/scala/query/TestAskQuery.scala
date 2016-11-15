package query

import globals.MyDatasets
import org.scalatest.FunSuite
import query.specific.QueryFactory
import query.variables.StaticQueryVariable
import rdf.SimpleRDF

/**
  * Created by Espen on 02.11.2016.
  */
class TestAskQuery extends FunSuite{
  test("In total there should be: # of properties") {
    QueryFactory.dataset = MyDatasets.SimilarProperties
    val result = QueryFactory.ask(new SimpleRDF(new StaticQueryVariable("http://www.w3.org/2002/07/owl#Property"), new StaticQueryVariable("http://www.w3.org/2000/01/rdf-schema#subClassOf"),
      new StaticQueryVariable("http://www.w3.org/2002/07/owl#Property")))

    print(result)
    assert(result == true)
  }


}
