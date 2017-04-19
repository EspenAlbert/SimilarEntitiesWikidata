package iAndO.factoryMethods

import core.globals.{ItemPropertyType, KnowledgeGraph}
import core.globals.KnowledgeGraph.KnowledgeGraph
import iAndO.dump.DumpObject

/**
  * Created by espen on 19.04.17.
  */
object IOFactory {
  def getAllItemProperties(implicit knowledgeGraph : KnowledgeGraph) : List[String] = {
    val propertiesToPropTypeMap = DumpObject.readJsonMapStringPropertyType(KnowledgeGraph.getMapPropToPropTypeFilename(knowledgeGraph))
    val itemProperties = (for {
      (propName, ItemPropertyType()) <- propertiesToPropTypeMap
    } yield propName).toList
    assert(itemProperties.size < propertiesToPropTypeMap.keys.size)
    assert(itemProperties.size > 100)
    itemProperties

  }

}
