package preprocessing.ownOntologyPopularizer

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals._
import core.globals.SimilarPropertyOntology.{rdfType, w}
import core.query.specific.{AskQuery, QueryFactory}
import iAndO.dump.DumpObject

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex
/**
  * Created by Espen on 04.11.2016.
  */
object MapPropertiesToPropTypes {

  def getPropertiesByReadingFile(): List[String] = {
    return Source.fromFile("output/domainCounts.nt").getLines().map{l =>
      l.split(" ")(0).drop(1).dropRight(1)
    }.toList
  }
  implicit val dataset = KnowledgeGraph.wikidata
  def filterGeoPropertyTypes(properties: List[String])(implicit dataset: KnowledgeGraph) : List[String] = {
    dataset match {
      case KnowledgeGraph.wikidata => {
        val geoProperty = ".*(P\\d+l[ao]).*".r
        return properties.filter{case geoProperty(pid) => true; case _ => false}
      }
      case KnowledgeGraph.dbPedia => {
        val geoProperty = ".*(http://www.w3.org/2003/01/geo/wgs84_pos#).*".r
        return properties.filter{case geoProperty(pid) => true; case _=> false}
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
        return properties
      }
    }
  }

  def findAllPropertyTypesAndTheirPropertyDatatype(propertiesInput: List[String]= Nil)(implicit dataset: KnowledgeGraph): Map[String, PropertyType] = {
    val propertyToPropertyDatatype = mutable.Map[String, PropertyType]()

    val properties = if(propertiesInput == Nil) QueryFactory.findAllDistinctProperties else propertiesInput
    val coordinateProperties = filterGeoPropertyTypes(properties)
    val ordinaryProperties = filterOrdinaryProperties(properties)
    for(p <- ordinaryProperties) {

      if (AskQuery.isItemProperty(p)) propertyToPropertyDatatype += (p -> ItemPropertyType())
      else {
        try {
          val datatypes = QueryFactory.findAllDistinctDatatypesForProperty(p)
          val datatypesInStringFormat = datatypes.map {
            PrimitiveDatatype.getDatatypeAsStringFromResult(_)
          }.
            filter {
              _.isDefined
            }.
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
          case a: Throwable => println(a); println(s"Had an error find datatype for: $p")
        }
      }
    }
    val mapGeoProps = coordinateProperties.map(prop => (prop -> GlobeCoordinatePropertyType())).toMap
    return propertyToPropertyDatatype.toMap ++ mapGeoProps
  }
  def dumpMappingToOwnOntologyDS(mapping : Map[String, PropertyType])(implicit dataset: KnowledgeGraph) = {
    DumpObject.dumpJsonMapStringPropertyType(mapping, s"$dataset-propToTypeMapping")
  }
}
