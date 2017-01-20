package query

import globals.SimilarPropertyOntology
import jenaQuerier.QueryLocalServer
import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.AttributeGenerator
import query.specific.UpdateQueryFactory
import rdf.SimpleRDFFactory

/**
  * Created by Espen on 01.12.2016.
  */
class TestUpdateSparql extends FunSuite {
  test("enable to add data to updateLocalData dataset") {
    UpdateQueryFactory.addToLocalDataset(SimpleRDFFactory.getStatement("w:Q76", "w:P31", "maniac"))
  }

  test("Delete all triples") {
    QueryLocalServer.deleteLocalData()
  }
  test("Adding a blank node to the graph") {
    val insertStatement = """insert { <http://www.wikidata.org/entity/Q76> <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchProperty> [
       <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchValue> <http://www.wikidata.org/entity/Q76>;
      <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchCount> "513" ] } where {}"""
    QueryLocalServer.updateLocalData(insertStatement);
  }
  test("Attribute generator updates as expected") {
    AttributeGenerator.generateValueCountsForProperty(true, "w:P43")
  }
  test("Attribute generator updates as expected also for range") {
    AttributeGenerator.generateValueCountsForProperty(false, "w:P106")
  }
}
