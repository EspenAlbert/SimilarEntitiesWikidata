package core.strategies

import core.feature.Feature
import core.globals.FeatureType
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.rdf.GraphRDF

import scala.collection.mutable

/**
  * Created by Espen on 11.11.2016.
  */
//case class InBNotInAGlobalStrategy(val queryEntityGraph : GraphRDF, override val weight : Double) extends Strategy {
//
//  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
//    val featureMap = mutable.Map[String, Feature]()
//    val propertiesForEntityGraph = queryEntityGraph.getProperties()
//    for (other <- otherEntities) {
//      val entity: String = other.entity
//      var count = 0
//      val propertiesOther = other.getProperties()
//      for (p <- propertiesOther) {
//        if (!propertiesForEntityGraph.contains(p)) count += 1
//      }
//      if (count > 0) {
//        featureMap += entity -> new Feature("Any", FeatureType.inBNotInAGlobal, count, weight)
//      }
//    }
//    return featureMap.toMap
//  }
//
//  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): List[String] = {
//    return Nil
//  }
//}