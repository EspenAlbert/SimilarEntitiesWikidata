package core.testData

import core.dump.DumpObject
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{ItemPropertyType, KnowledgeGraphs}

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

}
