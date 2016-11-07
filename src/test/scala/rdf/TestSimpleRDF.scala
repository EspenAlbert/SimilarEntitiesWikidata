package rdf

import org.scalatest.FunSuite

import scala.io.Source

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimpleRDF extends FunSuite{
  test("A statement must be printed correctly for n3 format") {
    val statement = new SimpleRDF("subject", "predicate", "object")
    assert(statement.getStatementNt() == "<subject> <predicate> <object>")
  }
  test("A file should be created properly") {
    val statement = new SimpleRDF("subject", "predicate", "object")
    CreateRdfFile.createRDFFile(List(statement), "test")
    val textFromFile = Source.fromFile("output/test.nt").getLines().next()
    assert(textFromFile == statement.getStatementNt() + ".")
  }
  test("A statement should be properly converted to sparql") {
    val statement = new SimpleRDF("w:Q76", "w:P21", "5000")
    assert(statement.getStatementNt() == "<http://www.wikidata.org/entity/Q76> <http://www.wikidata.org/entity/P21> \"5000\"")
  }


}
