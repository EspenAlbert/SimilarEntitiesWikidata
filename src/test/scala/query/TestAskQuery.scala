package query

import globals.MyDatasets
import org.scalatest.FunSuite
import query.specific.QueryFactory
import query.variables.StaticQueryVariable
import rdf.SimpleRDF
import query.AskQuery._

/**
  * Created by Espen on 02.11.2016.
  */
class TestAskQuery extends FunSuite{
  test("In total there should be: # of properties") {
    val result = QueryFactory.ask(new SimpleRDF(new StaticQueryVariable("http://www.w3.org/2002/07/owl#Property"), new StaticQueryVariable("http://www.w3.org/2000/01/rdf-schema#subClassOf"),
      new StaticQueryVariable("http://www.w3.org/2002/07/owl#Property")))
    print(result)
    assert(result == true)
  }
  test("same type possible should work") {
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1291") == false)
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1290") == true)
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1283") == false)
  }
  test("sharable domain should work") {
    assert(sharableDomain("http://www.wikidata.org/entity/P175") == true)
    assert(sharableDomain("http://www.wikidata.org/entity/P1283") == false)
  }
  test("sharable range should work") {
    assert(sharableRange("http://www.wikidata.org/entity/P175") == true)
    assert(sharableRange("http://www.wikidata.org/entity/P1283") == false)
  }


}
