package ownOntologyPopularizer.attributesGenerator

import globals.{ItemPropertyType, PropertyType}

/**
  * Created by espen on 20.02.17.
  */
object AttributeGeneratorv2 {
  def generateMetaStatementKnowledgeForProperties(propertiesToPropTypeMap : Map[String, PropertyType]) = {

    for((prop, propType) <- propertiesToPropTypeMap) {
      propType match {
        case a : ItemPropertyType =>
      }
    }
  }
}
