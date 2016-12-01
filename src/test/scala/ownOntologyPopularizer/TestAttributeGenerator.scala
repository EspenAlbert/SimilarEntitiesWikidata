package ownOntologyPopularizer


import globals.{PrimitiveDatatype, SimilarPropertyOntology}
import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.AttributeGenerator
import query.filters.NotEqualFilter
import query.specific.QueryFactory
import query.variables.{DynamicQueryVariable, OptionsForResultQueryVariable, StaticQueryVariable}
import rdf.{SimpleRDF, SimpleRDFFactory}

import scala.io.Source

/**
  * Created by Espen on 02.11.2016.
  */
class TestAttributeGenerator extends FunSuite{
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
//    assert(Source.fromFile("output/sharableDomain.nt").getLines().length == 752)


  }
  //P470 = true
  test("Test sharable range generator") {
    val secondObject = new DynamicQueryVariable("s2", false)
    secondObject.addQueryFilter(new NotEqualFilter(secondObject, "?s"))
    AttributeGenerator.generateStatementsForProperty(CustomPropertyClass.itemProperty, SimilarPropertyOntology.sharableRange,
      PrimitiveDatatype.boolean, "sharableRange", (p : String) => QueryFactory.ask(new SimpleRDF(p = new StaticQueryVariable(p), s = new DynamicQueryVariable("s", false)),
      new SimpleRDF(p = new StaticQueryVariable(p), s = secondObject)))
  } //TODO: Remove all lines where value = false -> See RemoveAllValuesWhereFalse...
  test("Test sameType possible generator") {
    val statement = ((s : String) => SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.unknownTypeFilter + "_" + "t", s, "?o " + OptionsForResultQueryVariable.unknownTypeFilter + "_t")))
    AttributeGenerator.generateStatementsForProperty(CustomPropertyClass.itemProperty, SimilarPropertyOntology.sameTypePossible,
      PrimitiveDatatype.boolean, "sameTypePossible", ((s) => QueryFactory.ask(statement(s))))
    assert(Source.fromFile("output/sameTypePossible.nt").getLines().length > 2)
  }
  test("The values in the domain are gatheredCorrectly for step father P43") {
    val subjects = AttributeGenerator.getDomainValuesWithMultipleObjects("w:P43")
    print(subjects)
    assert(subjects.contains("http://www.wikidata.org/entity/Q1124"))
    assert(subjects.length > 1)
  }
  test("The values in the range are gatheredCorrectly for step father P43") {
    val objects = AttributeGenerator.getRangeValuesWithMultipleSubjects("w:P43")
    print(objects)
    assert(objects.contains("http://www.wikidata.org/entity/Q43274"))
    assert(objects.length > 2)
  }
  test("The values in the range get a proper count") {
    val objects = AttributeGenerator.generateValueCountsForProperty(false, "w:P43")
  }
  test("The values in the domain get a proper count") {
    val objects = AttributeGenerator.generateValueCountsForProperty(true, "w:P43")
  }
  test("Create values for sharable domain ") {
    AttributeGenerator.valueCountsForSharableDomains()
  }




}
