package ownOntologyPopularizer.attributesGenerator

import query.specific.ontologyQueries.{FindAllPropertiesOfCustomClass, FindCountWhereQuery, FindIDForPropertyLabelQuery}
import rdf.{CreateRdfFile, SimpleRDF}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 04.11.2016.
  */
object CountAttributeGenerator {
  def generateCounts() = {
    val countIDquery = new FindIDForPropertyLabelQuery("count")
    countIDquery.execute()
    val countId = countIDquery.getValue()
    val propertiesWithoutCountQuery = new FindAllPropertiesOfCustomClass()
    propertiesWithoutCountQuery.execute()
    val properties = propertiesWithoutCountQuery.getSubjects()
    val statements = new ArrayBuffer[SimpleRDF]
    for(property <- properties){
      val countForPropertyQuery = new FindCountWhereQuery(new SimpleRDF(null, property, null))
      countForPropertyQuery.execute()
      val countForProperty = countForPropertyQuery.getValue()
      statements.append(new SimpleRDF(property, countId, countForProperty))
      print(s"Count for property: $property = $countForProperty ")
    }
    CreateRdfFile.createRDFFile(statements.toList, "countForProperties")
  }

}
