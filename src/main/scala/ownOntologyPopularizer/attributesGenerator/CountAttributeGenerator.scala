package ownOntologyPopularizer.attributesGenerator

import globals.{MyDatasets, SimilarPropertyOntology, PrimitiveDatatype}
import ownOntologyPopularizer.CustomPropertyClass
import query.specific.QueryFactory
import query.variables.StaticQueryVariable
import rdf.{CreateRdfFile, SimpleRDF}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 04.11.2016.
  */
object CountAttributeGenerator {
  def generateCounts() = {
    QueryFactory.dataset = MyDatasets.SimilarProperties
    val countId = SimilarPropertyOntology.spoCount.toString
    val properties = QueryFactory.findAllPropertiesOfCustomClass(CustomPropertyClass.baseProperty)
    val statements = new ArrayBuffer[SimpleRDF]
    QueryFactory.dataset = MyDatasets.Wikidata
    for(property <- properties){
      val countForProperty = QueryFactory.findTotalCountSubjectsWhereProperty(property)
      statements.append(new SimpleRDF(new StaticQueryVariable(property), new StaticQueryVariable(countId), new StaticQueryVariable(countForProperty.toString, PrimitiveDatatype.nonNegativeInteger)))
      print(s"Count for property: $property = $countForProperty ")
    }
    CreateRdfFile.createRDFFile(statements.toList, "countForProperties")
  }

}
