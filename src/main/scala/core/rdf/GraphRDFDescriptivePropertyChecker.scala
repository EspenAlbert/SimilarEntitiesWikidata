package core.rdf

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.QueryFactory
import core.strategies.StrategyFactory
import similarityFinder.MyConfiguration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

import scala.concurrent.duration._
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
  override lazy val getTypes : List[String] = {
    MyConfiguration.filterOnRdfType match {
      case false => statementsList.filter((s) => isType(s)).map(_._3)
      case _ => {
        val propertiesWhereSubject = Await.result(entityIsSubjectStatments,10 seconds)._2
        val propertiesWhereObject = Await.result(entityIsObjectStatements,10 seconds)._2
        QueryFactory.findOrderedCountForTypes(propertiesWhereSubject, propertiesWhereObject)
        .take(MyConfiguration.numberOfComparableTypes)


      }
    }
  }


}
