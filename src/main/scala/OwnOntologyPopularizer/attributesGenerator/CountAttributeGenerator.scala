package ownOntologyPopularizer.attributesGenerator

import globals.PrimitiveDatatype
import query.specific.QueryFactory
import query.variables.StaticQueryVariable
import rdf.{CreateRdfFile, SimpleRDF}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 04.11.2016.
  */
object CountAttributeGenerator {
  def generateCounts() = {
    val countId = QueryFactory.findIDForPropertyLabelQuery("count")
    val properties = QueryFactory.findAllPropertiesOfCustomClass()
    val statements = new ArrayBuffer[SimpleRDF]
    for(property <- properties){
      val countForProperty = QueryFactory.findTotalCountWhereProperty(property)
      statements.append(new SimpleRDF(new StaticQueryVariable(property), new StaticQueryVariable(countId), new StaticQueryVariable(countForProperty.toString, PrimitiveDatatype.uint)))
      print(s"Count for property: $property = $countForProperty ")
    }
    CreateRdfFile.createRDFFile(statements.toList, "countForProperties")
  }

}
