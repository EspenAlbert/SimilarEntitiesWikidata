package ownOntologyPopularizer

import dump.DumpObject
import globals.{PrimitiveDatatype, PropertyType, SimilarPropertyOntology}
import globals.SimilarPropertyOntology.{rdfType, w}
import query.specific.{QueryFactory, QueryFactoryRaw}
import query.variables.StaticQueryVariable
import rdf.{CreateRdfFile, SimpleRDF}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
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
  val ordinaryPropertiesPattern = "http://www.wikidata.org/entity/P\\d+".r

  val dummyTest = true
  def getPropertiesByReadingFile(): List[String] = {
    return Source.fromFile("output/domainCounts.nt").getLines().map{l =>
      l.split(" ")(0).drop(1).dropRight(1)
    }.toList
  }

  def findAllPropertyTypesAndTheirPropertyDatatype(): Map[String, PropertyType] = {
    val propertyToPropertyDatatype = mutable.Map[String, PropertyType]()

    val properties = if(!dummyTest) QueryFactoryRaw.findAllDistinctProperties else getPropertiesByReadingFile()
    val coordinateProperties = properties.filter{case geoProperty(pid) => true; case _ => false}
    val ordinaryProperties = properties.filter{p => ordinaryPropertiesPattern.findFirstIn(p).isInstanceOf[Some[String]]}
    for(p <- ordinaryProperties) {
      try {
        val datatypes = QueryFactoryRaw.findAllDistinctDatatypesForProperty(p)
        val datatypesInStringFormat = datatypes.map {
          PrimitiveDatatype.getDatatypeAsStringFromResult(_)
        }.
          filter { case Some(s) => true; case _ => false }.
          map { case Some(s) => s }
        PrimitiveDatatype.getPropertyTypeFromDatatypes(datatypesInStringFormat) match {
          case Some(pType) => propertyToPropertyDatatype += (p -> pType)
          case None => {
            PrimitiveDatatype.determineFromObjectValuePropertyType(p) match {
              case Some(pType) => propertyToPropertyDatatype += (p -> pType)
              case None => println(s"Unable to determine property datatype for property=$p")
            }

          }
        }
      } catch {
        case a : Throwable => println(a); println(s"Had an error find datatype for: $p")
      }
    }
    return propertyToPropertyDatatype.toMap
  }
  def dumpMappingToOwnOntologyDS(mapping : Map[String, PropertyType]) = {
    DumpObject.dumpJsonMapStringPropertyType(mapping, "propToTypeMapping")
  }
}
