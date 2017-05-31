package core.rdf

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.QueryFactory
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
/**
  * Created by espen on 18.04.17.
  */
class GraphRDFDescriptivePropertyChecker(entity: String)(implicit knowledgeGraph: KnowledgeGraph, useMustHaveProperty : Boolean = false, filterOnRdfType : Boolean = false, numberOfComparableTypes : Int = 10) extends GraphRDF(entity){
  override lazy val entityIsObjectStatements = Future{
    val propertiesWhereObject = QueryFactory.findLowCountPropertiesWhereEntityIsObject(entity)
    val pairs = {for {
        prop <- propertiesWhereObject
//        if (!StrategyFactory.isDescriptive(prop) || StrategyFactory.valueIsAPotentialValueMatchFindCount(entity, prop, false).get < MyConfiguration.thresholdCountCheapStrategy)
        otherEntities = QueryFactory.subjectsWithPropertyAndValue(prop, entity, true)(knowledgeGraph,mustHaveProperty = "", mustHavePropertyIsSubject = false)
      } yield otherEntities.map(e => (e, prop))}.flatten
    (pairs.map(_._1), pairs.map(_._2))
  }
  override lazy val getTypes : List[String] = {
    if(useMustHaveProperty) {
      val propertiesWhereSubject = Await.result(entityIsSubjectStatments, 10 seconds)._2
      val propertiesWhereObject = Await.result(entityIsObjectStatements, 10 seconds)._2
      val highestPropertyCountWhereSubject = GraphRDFDescriptivePropertyChecker.findMaxCount(propertiesWhereSubject)
      val highestPropertyCountWhereObject = GraphRDFDescriptivePropertyChecker.findMaxCount(propertiesWhereObject)
      val isSubject = highestPropertyCountWhereSubject._2 > highestPropertyCountWhereObject._2
      implicit val mustHaveProperty = if(isSubject) highestPropertyCountWhereSubject._1 else highestPropertyCountWhereObject._1
      implicit val mustHavePropertyIsSubject = isSubject

    }
    if (filterOnRdfType) {
      val propertiesWhereSubject = Await.result(entityIsSubjectStatments, 10 seconds)._2
      val propertiesWhereObject = Await.result(entityIsObjectStatements, 10 seconds)._2
      QueryFactory.findOrderedCountForTypes(propertiesWhereSubject, propertiesWhereObject, numberOfComparableTypes)
        .take(numberOfComparableTypes)
    }else {
      statementsList.filter((s) => isType(s)).map(_._3)
    }
  }



}

object GraphRDFDescriptivePropertyChecker {
  def findMaxCount(properties: List[String]) = {
    properties
      .groupBy(p => p)
      .map(tuple => tuple._1 -> tuple._2.size)
      .maxBy(_._2)
  }
}
