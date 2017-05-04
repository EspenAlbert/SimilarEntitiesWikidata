package query

import core.globals.{MyDatasets, SimilarPropertyOntology}
import core.query.specific.UpdateQueryFactory
import jenaQuerier.QueryLocalServer
import org.scalatest.FunSuite

/**
  * Created by Espen on 01.12.2016.
  */
class TestUpdateSparql extends FunSuite {
  test("Delete all triples") {
    QueryLocalServer.deleteLocalData()
  }
  test("Adding a blank node to the graph") {
    val insertStatement = """insert { <http://www.wikidata.org/entity/Q76> <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchProperty> [
       <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchValue> <http://www.wikidata.org/entity/Q76>;
      <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchCount> "513" ] } where {}"""
    QueryLocalServer.updateLocalData(insertStatement, MyDatasets.valueMatchWikidata);
  }
}
