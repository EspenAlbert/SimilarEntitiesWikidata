package query

import org.scalatest.FunSuite
import query.specific.QueryFactory
import query.variables.{OptionsForResultQueryVariable, StaticQueryVariable}
import rdf.{SimpleRDF, SimpleRDFFactory}

/**
  * Created by Espen on 02.11.2016.
  */
class TestWhereFilter extends FunSuite{
  test("A select with multiple of same selects should be reduced") {
    val reducedSelect: String = WhereFilter.filterAwayMultipleOfSameSelect("select ?s ?s distinct ?p distinct ?p")
    print(reducedSelect)
    assert(reducedSelect == "select ?s distinct ?p")
  }
  test("Where filter should still be able to count...") {
    val statemetn = SimpleRDFFactory.getStatement("?s " + OptionsForResultQueryVariable.count, "w:P43", "w:Q76")
    val whereStatement = new WhereFilter(statemetn)
    val reducedSelect: String = WhereFilter.filterAwayMultipleOfSameSelect("select (count(?s) as ?c) ?s ?s distinct ?p distinct ?p")
    print(reducedSelect)
//    print(whereStatement.getSelect())
  }


}
