package preprocessing.ownOntologyPopularizer

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.{AskQuery, QueryFactory}
import core.strategies.StrategyFactory
import similarityFinder.MyConfiguration

/**
  * Created by espen on 18.04.17.
  */
object IsDescriptivePropertyClassifier {
  def classify(properties : List[String])(implicit knowledgeGraph: KnowledgeGraph) : List[Boolean] = {
    val sFactory = new StrategyFactory()
    properties.map(p => {
      val domainCount = sFactory.mapPropertyToDomainCounts.get(p)
      val rangeCount = sFactory.mapPropertyToRangeCounts.get(p)
      (domainCount,rangeCount) match {
        case (Some(dCount), Some(rCount)) if (dCount.toDouble / rCount) > MyConfiguration.thresholdForBeingDescriptiveProperty => true
        case _ => false
      }
    })
  }

}
