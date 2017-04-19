package core.rdf

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.QueryFactory
import core.strategies.StrategyFactory
import similarityFinder.MyConfiguration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by espen on 18.04.17.
  */
class GraphRDFDescriptivePropertyChecker(entity: String)(implicit knowledgeGraph: KnowledgeGraph) extends GraphRDF(entity){
  override lazy val entityIsObjectStatements = Future{
    val propertiesWhereObject = QueryFactory.findDistinctPropertiesWhereObject(entity)
    val pairs = {for {
        prop <- propertiesWhereObject
        if (!StrategyFactory.isDescriptive(prop) || StrategyFactory.valueIsAPotentialValueMatchFindCount(entity, prop, false).get < MyConfiguration.thresholdCountCheapStrategy)
        otherEntities = QueryFactory.subjectsWithPropertyAndValue(prop, entity)
      } yield otherEntities.map(e => (e, prop))}.flatten
    (pairs.map(_._1), pairs.map(_._2))
  }


}
