package rdf

import globals.PrimitiveDatatype
import org.scalatest.FunSuite
import query.variables.OptionsForResultQueryVariable

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimpleRDFFactory extends FunSuite{
  test("Check that static query variables are created properly") {
    assert(SimpleRDFFactory.getStatement(("w:Q76", "w:P21", "true " + PrimitiveDatatype.boolean)).wherePhrase() == "<http://www.wikidata.org/entity/Q76> <http://www.wikidata.org/entity/P21> \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> .\n")
  }
  test("Check that dynamic query variable are created properly and same type filter works") {
    assert(SimpleRDFFactory.getStatement(("?s", "w:P21", "?o " + OptionsForResultQueryVariable.sameTypeFilter + "_w:Q5")).wherePhrase() ==
      "?s <http://www.wikidata.org/entity/P21> ?o .\n?o <http://www.wikidata.org/entity/P31> <http://www.wikidata.org/entity/Q5> .")
  }
  test("Check string language filter") {
    assert(SimpleRDFFactory.getStatement(("?s", "w:P21", "?o " + OptionsForResultQueryVariable.sameLanguageFilter + "_eqUAlName_en")).wherePhrase() ==
      "?s <http://www.wikidata.org/entity/P21> ?o .\nfilter(?o = \"eqUAlName\"@en)")
  }
  test("Check not equal filter") {
    assert(SimpleRDFFactory.getStatement(("?s", "w:P21", "?o " + OptionsForResultQueryVariable.notEqualFilter + "_?o2")).wherePhrase() ==
      "?s <http://www.wikidata.org/entity/P21> ?o .\nfilter(?o != ?o2)")
  }

}
