package ownOntologyPopularizer

import globals.{PrimitiveDatatype, SimilarPropertyOntology}
import globals.SimilarPropertyOntology.{PropertyTypes, rdfType, w}
import query.specific.{QueryFactory, QueryFactoryRaw}
import query.variables.StaticQueryVariable
import rdf.{CreateRdfFile, SimpleRDF}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
/**
  * Created by Espen on 04.11.2016.
  */
object InsertPropertiesIntoOntology {
  def insertProperties() = {
    val properties = QueryFactory.findAllDistinctProperties
    val propertiesWithoutPrefix = for(s <- properties.filter(_.indexOf("P") > 0)) yield s.substring(s.indexOf("P"))
    val propertyDatatypeMap = PropertyDatatypeReader.getPropertyDatatypeMap()
    val datatypePropertyClass = CustomPropertyClass.datatypeToClass
    val statements = ArrayBuffer[SimpleRDF]()
    val deletedProperties = Set("P1224", "P513", "P1223", "P1231", "P1226", "P2608")
    for(property <- propertiesWithoutPrefix.filterNot((p) => !p.startsWith("P") || p.length > 5 || deletedProperties.contains(p))) {
      try {
        val datatype = propertyDatatypeMap(property)
        val propertyClass = datatypePropertyClass(datatype)
        val actualClassId = SimilarPropertyOntology.spo.toString + "#" + propertyClass
        statements.append(new SimpleRDF(new StaticQueryVariable(w + property), new StaticQueryVariable(rdfType.toString), new StaticQueryVariable(actualClassId)))
      } catch {
        case _ : Throwable => print("could not find: " + property)
      }
    }
    CreateRdfFile.createRDFFile(statements.toList, "propertyMappedToOntology")
  }

  val geoProperty = ".*(P\\d+l[ao]).*".r
  val ordinaryPropertiesPattern = "<http://www.wikidata.org/entity/P\\d+>".r
  def findAllPropertyTypesAndTheirPropertyDatatype() = {
    val propertyToPropertyDatatype = mutable.Map[String, PropertyTypes]()

    val properties = QueryFactory.findAllDistinctProperties
    val coordinateProperties = properties.filter{case geoProperty(pid) => true; case _ => false}
    val ordinaryProperties = properties.filter{case ordinaryPropertiesPattern(prop) => true; case _=> false}
    for(p <- ordinaryProperties) {
      val datatypes = QueryFactoryRaw.findAllDistinctDatatypesForProperty(p)
      val datatypesInStringFormat = datatypes.map{PrimitiveDatatype.getDatatypeAsStringFromResult(_)}.
        filter{case Some(s) => true; case _ => false}.
        map{case Some(s) => s}
      PrimitiveDatatype.getPropertyTypeFromDatatypes(datatypesInStringFormat) match {
        case Some(pType) => propertyToPropertyDatatype += (p -> pType)
        case None => {
          PrimitiveDatatype.determineFromObjectValuePropertyType(p) match {
            case Some(pType) => propertyToPropertyDatatype += (p -> pType)
            case None => println(s"Unable to determine property datatype for property=$p")
          }

        }
      }
    }
    //Create the coordinate property class for properties with suffix la, lo
    //Filter all properties who is on the form: <http://www.wikidata.org/entity/P1549>
    //Find all possible datatypes for each property.


  }
}
