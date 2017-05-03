package core.testData

import core.dump.DumpObject
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{ItemPropertyType, KnowledgeGraphs, PropertyType}
import core.query.specific.QueryFactory

import scala.util.{Failure, Success, Try}

/**
  * Created by espen on 19.04.17.
  */
object IOFactory {
  def getAllItemProperties(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val propertiesToPropTypeMap = DumpObject.readJsonMapStringPropertyType(KnowledgeGraphs.getMapPropToPropTypeFilename(knowledgeGraph))
    val itemProperties = (for {
      (propName, ItemPropertyType()) <- propertiesToPropTypeMap
    } yield propName).toList
    assert(itemProperties.size < propertiesToPropTypeMap.keys.size)
    assert(itemProperties.size > 100)
    itemProperties
  }
  private def filenamePropToDomainCount(implicit knowledgeGraph : KnowledgeGraph) = s"$knowledgeGraph-prop-domain-count"
  private def filenamePropToRangeCount(implicit knowledgeGraph : KnowledgeGraph) = s"$knowledgeGraph-prop-range-count"
  private def filenamePropToIsDescriptive(implicit knowledgeGraph : KnowledgeGraph) = s"$knowledgeGraph-prop-is-descriptive"

  def getIsDescriptive(implicit knowledgeGraph : KnowledgeGraph): Map[String, Boolean] = {
    Try(DumpObject.getMapStringBoolean(filenamePropToIsDescriptive)) match {
      case Success(mapped) => mapped
      case Failure(_) => {
        val (properties, isDescriptive) = QueryFactory.findIsDescriptive()
        val mapped = properties.zip(isDescriptive).toMap
        DumpObject.dumpMapStringBoolean(mapped, filenamePropToIsDescriptive)
        return mapped
      }
    }
  }
  def getDomainCounts(implicit knowledgeGraph : KnowledgeGraph): Map[String, Int] = {
      Try(DumpObject.getMapStringInt(filenamePropToDomainCount)) match {
        case Success(mapped) => mapped
        case Failure(_) => {
          val (properties, strategies) = QueryFactory.findAllDomainCounts()
          val mapped = properties.zip(strategies).toMap
          DumpObject.dumpMapStringInt(mapped, filenamePropToDomainCount)
          return mapped
        }
      }
  }
  def getRangeCounts(implicit knowledgeGraph : KnowledgeGraph): Map[String, Int] = {
    Try(DumpObject.getMapStringInt(filenamePropToRangeCount)) match {
      case Success(mapped) => mapped
      case Failure(_) => {
        val (properties, strategies) = QueryFactory.findAllRangeCounts()
        val mapped = properties.zip(strategies).toMap
        DumpObject.dumpMapStringInt(mapped, filenamePropToRangeCount)
        return mapped
      }
    }
  }
  private def filenamePropertyToPropType(knowledgeGraph: KnowledgeGraph) = s"$knowledgeGraph-propToTypeMapping"

  def dumpPropertyToProptypeMapping(mapping : Map[String, PropertyType])(implicit dataset: KnowledgeGraph) = {
    DumpObject.dumpJsonMapStringPropertyType(mapping, filenamePropertyToPropType(dataset))
  }
  def getPropertyToPropType(implicit knowledgeGraph : KnowledgeGraph): Map[String, PropertyType] = {
    return DumpObject.readJsonMapStringPropertyType(filenamePropertyToPropType(knowledgeGraph))
  }

}
