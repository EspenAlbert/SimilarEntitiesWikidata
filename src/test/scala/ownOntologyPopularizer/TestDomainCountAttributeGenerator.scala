package ownOntologyPopularizer


import globals.{PrimitiveDatatype, SimilarPropertyOntology}
import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.AttributeGenerator
import query.filters.NotEqualFilter
import query.specific.QueryFactory
import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.SimpleRDF

import scala.io.Source

/**
  * Created by Espen on 02.11.2016.
  */
class TestDomainCountAttributeGenerator extends FunSuite{
  test("All properties should have a domain count except 3 most common") {
    AttributeGenerator.generateStatementsForProperty(CustomPropertyClass.baseProperty, SimilarPropertyOntology.domainCount, PrimitiveDatatype.nonNegativeInteger, "domainCounts", QueryFactory.findTotalCountSubjectsWhereProperty(_, true))
    assert(Source.fromFile("output/domainCounts.nt").getLines().length > 752)
  }
  test("Test range count generator") {
    AttributeGenerator.generateStatementsForProperty(CustomPropertyClass.itemProperty, SimilarPropertyOntology.rangeCount,
    PrimitiveDatatype.nonNegativeInteger, "rangeCounts", QueryFactory.findTotalCountObjectsWhereProperty(_, true))
    assert(Source.fromFile("output/rangeCounts.nt").getLines().length == 752)
  }
  ////P470 = false
  test("Test sharable domain generator") {
    val secondObject = new DynamicQueryVariable("o2", false)
    secondObject.addQueryFilter(new NotEqualFilter(secondObject, "?o"))
    AttributeGenerator.generateStatementsForProperty(CustomPropertyClass.itemProperty, SimilarPropertyOntology.sharableDomain,
      PrimitiveDatatype.boolean, "sharableDomain", (p : String) => QueryFactory.ask(new SimpleRDF(p = new StaticQueryVariable(p), o = new DynamicQueryVariable("o", false)),
      new SimpleRDF(p = new StaticQueryVariable(p), o = secondObject)))
  }
  //P470 = true
  test("Test sharable range generator") {
    val secondObject = new DynamicQueryVariable("s2", false)
    secondObject.addQueryFilter(new NotEqualFilter(secondObject, "?s"))
    AttributeGenerator.generateStatementsForProperty(CustomPropertyClass.itemProperty, SimilarPropertyOntology.sharableRange,
      PrimitiveDatatype.boolean, "sharableRange", (p : String) => QueryFactory.ask(new SimpleRDF(p = new StaticQueryVariable(p), s = new DynamicQueryVariable("s", false)),
      new SimpleRDF(p = new StaticQueryVariable(p), s = secondObject)))
  } //TODO: Remove all lines where value = false


}
