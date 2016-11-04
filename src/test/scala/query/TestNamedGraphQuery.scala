package query

import org.scalatest.FunSuite


/**
  * Created by Espen on 02.11.2016.
  */
class TestNamedGraphQuery extends FunSuite{
  test("query formulated to named graph") {
    val expectedQuery = "select ?subject ?predicate ?object\nFROM <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology>\nWHERE {\n  ?subject ?predicate ?object\n}\nLIMIT 25"
    val namedGraph = "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology"
    val query = "select ?subject ?predicate ?object\nWHERE {\n  ?subject ?predicate ?object\n}\nLIMIT 25"
    assert(NamedGraphQuery.getQuery(query, namedGraph) == expectedQuery)
  }

}
