package ownOntologyPopularizer.attributesGenerator

import globals.{MyDatasets, SimilarPropertyOntology}
import globals.PrimitiveDatatype.PrimitiveDatatype
import globals.SimilarPropertyOntology.SimilarPropertyOntology
import jenaQuerier.QueryLocalServer
import ownOntologyPopularizer.CustomPropertyClass.CustomPropertyClass
import query.filters.NotEqualFilter
import query.specific.{QueryFactory, QueryFactoryV2}
import query.variables.{DynamicQueryVariable, OptionsForResultQueryVariable, StaticQueryVariable}
import rdf.{CreateRdfFile, SimpleRDF, SimpleRDFFactory, WikidataPropertyHelper}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 11.11.2016.
  */
object AttributeGenerator {
  def generateStatementsForProperty(domainOfProperties: CustomPropertyClass, customPropertyName: SimilarPropertyOntology, primitiveDatatype: PrimitiveDatatype, filename : String, queryFunction: String => Any) = {
    val properties = QueryFactory.findAllPropertiesOfCustomClass(domainOfProperties)
    val statements = new ArrayBuffer[SimpleRDF]
    val tooCommonProperties = Set("P31", "P17", "P131")
    for(property <- properties.filterNot((p) => tooCommonProperties.contains(p.substring(p.indexOf("P"))))){
      val valueForProperty = queryFunction(property)
      statements.append(new SimpleRDF(new StaticQueryVariable(property), new StaticQueryVariable(customPropertyName.toString), new StaticQueryVariable(valueForProperty.toString, primitiveDatatype)))
      println(s"Value for property: $property = $valueForProperty for $customPropertyName")
    }
    CreateRdfFile.createRDFFile(statements.toList, filename)
  }
  def valueCountsForSharableDomains() : Unit = {
    val properties : List[String] = QueryFactoryV2.findList(SimpleRDFFactory.getStatement("?s", SimilarPropertyOntology.sharableDomain, "?o " + OptionsForResultQueryVariable.ignoreMe))
    print(properties.length)
    for(prop <- properties) {
      generateValueCountsForProperty(true, prop)
    }

  }


  def generateValueCountsForProperty(isDomain : Boolean, property:String): Unit = {
    val subjectsOrObjectsWithPotentialValueMatch = if(isDomain) getDomainValuesWithMultipleObjects(property) else getRangeValuesWithMultipleSubjects(property)
    println(subjectsOrObjectsWithPotentialValueMatch.length, "Have multiple subj/obj")
    val propertyAsFullString = SimilarPropertyOntology.w +  property.substring(property.indexOf("P"))
    for(entity <- subjectsOrObjectsWithPotentialValueMatch) {
      val countQuery = if(isDomain) SimpleRDFFactory.getStatement(entity, property, "?o " + OptionsForResultQueryVariable.count) else
        SimpleRDFFactory.getStatement("?s " + OptionsForResultQueryVariable.count, property, entity)
      val count : Int = QueryFactoryV2.findSingleValue(countQuery)
      val updateQuery = s"insert { <$propertyAsFullString> <${SimilarPropertyOntology.valueMatchProperty}> [ <${SimilarPropertyOntology.valueMatchValue}> <$entity>;\n" +
      s"""<${SimilarPropertyOntology.valueMatchCount}> "%d" ] } where {}""".format(count)
      QueryLocalServer.updateLocalData(updateQuery)
    }
  }

  def getDomainValuesWithMultipleObjects(property: String): List[String] = {
    val subjects: DynamicQueryVariable = DynamicQueryVariable("s", true)
    val propertyInQuery: StaticQueryVariable = StaticQueryVariable(property)
    val findDomains = new SimpleRDF(s = subjects, p = propertyInQuery, o = DynamicQueryVariable("o", false, true))
    val secondObject = new DynamicQueryVariable("o2", false, true)
    secondObject.addQueryFilter(new NotEqualFilter(secondObject, "?o"))
    QueryFactoryV2.findList(findDomains, new SimpleRDF(s = subjects, p = propertyInQuery, o = secondObject))
  }
  def getRangeValuesWithMultipleSubjects(property: String): List[String] = {
    val objects: DynamicQueryVariable = DynamicQueryVariable("o", true)
    val propertyInQuery: StaticQueryVariable = StaticQueryVariable(property)
    val findDomains = new SimpleRDF(o = objects, p = propertyInQuery, s = DynamicQueryVariable("s", false, true))
    val secondObject = new DynamicQueryVariable("s2", false, true)
    secondObject.addQueryFilter(new NotEqualFilter(secondObject, "?s"))
    return QueryFactoryV2.findList(findDomains, new SimpleRDF(o = objects, p = propertyInQuery, s = secondObject))
  }
}
