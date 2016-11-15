package ownOntologyPopularizer.attributesGenerator

import globals.MyDatasets
import globals.PrimitiveDatatype.PrimitiveDatatype
import globals.SimilarPropertyOntology.SimilarPropertyOntology
import ownOntologyPopularizer.CustomPropertyClass.CustomPropertyClass
import query.specific.QueryFactory
import query.variables.StaticQueryVariable
import rdf.{CreateRdfFile, SimpleRDF}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 11.11.2016.
  */
object AttributeGenerator {
  def generateStatementsForProperty(domainOfProperties: CustomPropertyClass, customPropertyName: SimilarPropertyOntology, primitiveDatatype: PrimitiveDatatype, filename : String, queryFunction: String => Any) = {
    val properties = QueryFactory.findAllPropertiesOfCustomClass(domainOfProperties)
    val statements = new ArrayBuffer[SimpleRDF]
    QueryFactory.dataset = MyDatasets.Wikidata
    val tooCommonProperties = Set("P31", "P17", "P131")
    for(property <- properties.filterNot((p) => tooCommonProperties.contains(p.substring(p.indexOf("P"))))){
      val valueForProperty = queryFunction(property)
      statements.append(new SimpleRDF(new StaticQueryVariable(property), new StaticQueryVariable(customPropertyName.toString), new StaticQueryVariable(valueForProperty.toString, primitiveDatatype)))
      println(s"Value for property: $property = $valueForProperty for $customPropertyName")
    }
    CreateRdfFile.createRDFFile(statements.toList, filename)
  }
}
