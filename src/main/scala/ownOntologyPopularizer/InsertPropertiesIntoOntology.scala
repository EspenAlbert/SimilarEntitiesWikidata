package ownOntologyPopularizer

import globals.Namespace.{rdfType, w}
import query.specific.QueryFactory
import query.variables.StaticQueryVariable
import rdf.{CreateRdfFile, SimpleRDF}

import scala.collection.mutable.ArrayBuffer
/**
  * Created by Espen on 04.11.2016.
  */
object InsertPropertiesIntoOntology {
  def insertProperties() = {
    val properties = QueryFactory.findAllDistinctProperties
    val propertiesWithoutPrefix = for(s <- properties) yield s.drop(2)
    val propertyDatatypeMap = PropertyDatatypeReader.getPropertyDatatypeMap()
    val datatypePropertyClass = MapPropertyDatatypeToClass.datatypeToClass
    val statements = ArrayBuffer[SimpleRDF]()
    val deletedProperties = Set("P1224", "P513", "P1223", "P1231", "P1226", "P2608")
    for(property <- propertiesWithoutPrefix.filterNot((p) => !p.startsWith("P") || p.length > 5 || deletedProperties.contains(p))) {
      try {
        val datatype = propertyDatatypeMap(property)
        val propertyClass = datatypePropertyClass(datatype)
        val actualClassId = QueryFactory.findIDForPropertyLabelQuery(propertyClass)
        statements.append(new SimpleRDF(new StaticQueryVariable(w + property), new StaticQueryVariable(rdfType.toString), new StaticQueryVariable(actualClassId)))
      } catch {
        case _ : Throwable => print("could not find: " + property)
      }
    }
    CreateRdfFile.createRDFFile(statements.toList, "propertyMappedToOntology")
  }
}
