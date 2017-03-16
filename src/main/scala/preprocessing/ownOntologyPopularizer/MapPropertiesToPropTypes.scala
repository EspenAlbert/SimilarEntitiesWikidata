package preprocessing.ownOntologyPopularizer

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals._
import core.globals.SimilarPropertyOntology.{rdfType, w}
import core.query.specific.QueryFactory
import iAndO.dump.DumpObject

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex
/**
  * Created by Espen on 04.11.2016.
  */
object MapPropertiesToPropTypes {

  val dummyTest = false
  def getPropertiesByReadingFile(): List[String] = {
    return Source.fromFile("output/domainCounts.nt").getLines().map{l =>
      l.split(" ")(0).drop(1).dropRight(1)
    }.toList
  }
  implicit val dataset = KnowledgeGraph.wikidata
  def filterGeoPropertyTypes(properties: List[String])(implicit dataset: KnowledgeGraph) : List[String] = {
    dataset match {
      case KnowledgeGraph.wikidata => {
        val geoProperty = ".*(P\\d+q?l[ao]).*".r
        return properties.filter{case geoProperty(pid) => true; case _ => false}
      }
      case KnowledgeGraph.dbPedia => {
        throw new NotImplementedError()
      }
    }
  }
  def filterOrdinaryProperties(properties: List[String])(implicit dataset: KnowledgeGraph): List[String] = {
    dataset match {
      case KnowledgeGraph.wikidata => {
        val ordinaryPropertiesPattern = "http://www.wikidata.org/entity/P\\d+$".r
        return properties.filter{p => ordinaryPropertiesPattern.findFirstIn(p).isInstanceOf[Some[String]]}
      }
      case KnowledgeGraph.dbPedia => {
        throw new NotImplementedError()
      }
    }
  }

  def findAllPropertyTypesAndTheirPropertyDatatype(): Map[String, PropertyType] = {
    val propertyToPropertyDatatype = mutable.Map[String, PropertyType]()

    val properties = if(dummyTest) getPropertiesByReadingFile() else QueryFactory.findAllDistinctProperties
    val coordinateProperties = filterGeoPropertyTypes(properties)
    val ordinaryProperties = filterOrdinaryProperties(properties)
    println(s"Coordinate properties: $coordinateProperties")
    println(s"ordinaryProperties: $ordinaryProperties")
    println(s"Other properties: ${properties.filterNot(coordinateProperties.contains(_)).filterNot(ordinaryProperties.contains(_))}")
    for(p <- ordinaryProperties) {
      try {
        val datatypes = QueryFactory.findAllDistinctDatatypesForProperty(p)
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
    DumpObject.dumpJsonMapStringPropertyType(mapping, "propToTypeMapping2")
  }
}
