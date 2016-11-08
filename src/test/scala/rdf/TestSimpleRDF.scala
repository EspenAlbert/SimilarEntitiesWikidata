package rdf

import org.scalatest.FunSuite
import query.variables.StaticQueryVariable

import scala.io.Source

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimpleRDF extends FunSuite{
  test("A file should be created properly") {
    val statement = new SimpleRDF(new StaticQueryVariable("subject"), new StaticQueryVariable("predicate"), new StaticQueryVariable("object"))
    CreateRdfFile.createRDFFile(List(statement), "test")
    val textFromFile = Source.fromFile("output/test.nt").getLines().next()
    assert(textFromFile == statement.getStatementNt())
  }
  test("A statement should be properly converted to sparql") {
    val statement = new SimpleRDF(new StaticQueryVariable("w:Q76"), new StaticQueryVariable("w:P21"), new StaticQueryVariable("5000"))
    assert(statement.getStatementNt() == "<http://www.wikidata.org/entity/Q76> <http://www.wikidata.org/entity/P21> \"5000\".")
  }


}
