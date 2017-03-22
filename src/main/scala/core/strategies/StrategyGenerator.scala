package core.strategies

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.SimilarPropertyOntology
import core.rdf.GraphRDF
import similarityFinder.MyConfiguration

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 09.11.2016.
  */
object StrategyGenerator {

  def statementHasProperty(prop: String, statement :Tuple3[String, String, String]) : Boolean = {
    statement match {
      case (s, `prop`, o) => true
      case _ => false
    }

  }

  def generateStrategies(entityGraph: GraphRDF)(implicit knowledgeGraph: KnowledgeGraph): Array[Strategy] = {
    val strategies = new ArrayBuffer[Strategy]()
    for(prop <- entityGraph.getUniqueWikidataPropertiesWithoutTheMostCommon()) {
      val masterStrategy = new MasterStrategy(entityGraph.statements.filter((s) => statementHasProperty(prop, s)).toList, entityGraph.entity, entityGraph.getTypes)
      strategies.append(masterStrategy.getCompositeStrategies() : _*)
    }
//    if(MyConfiguration.globalInBNotInAActive) {
//      strategies.append(InBNotInAGlobalStrategy(entityGraph, MyConfiguration.globalInBNotInABoost * MasterStrategy.logarithmicWeightForCount(SimilarPropertyOntology.maxCountForProperties.toString.toInt / 2)))
//    }
    return strategies.toArray
  }
}
