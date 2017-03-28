package preprocessing.ownOntologyPopularizer.attributesGenerator

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals._
import core.query.specific.{AskQuery, QueryFactory, UpdateQueryFactory}

import scala.collection.mutable

/**
  * Created by espen on 20.02.17.
  */
object PropTypeToStrategyCreator {
  def generateMetaStatementKnowledgeAndStrategiesForProperties(propertiesToPropTypeMap : Map[String, PropertyType])(implicit knowledgeGraph: KnowledgeGraph) = {
    val (propToDomainCount: mutable.Map[String, Int], propToRangeCount: mutable.Map[String, Int], sameTypePossibleProps: mutable.Set[String], sharableDomainProps: mutable.Set[String], sharableRangeProps: mutable.Set[String], dateTimeStrategies : mutable.Set[String]) = findMetaPropertyKnowledge(propertiesToPropTypeMap.filterNot(propToType => propToType._1 == KnowledgeGraph.getTypeProperty(knowledgeGraph)))
    addMetaKnowledgeToDatabase(propToDomainCount, propToRangeCount)
    addStrategiesToDatabase(propertiesToPropTypeMap, sameTypePossibleProps, sharableDomainProps, sharableRangeProps, dateTimeStrategies)
  }

  def addStrategiesToDatabase(propertiesToPropTypeMap: Map[String, PropertyType], sameTypePossibleProps: mutable.Set[String], sharableDomainProps: mutable.Set[String], sharableRangeProps: mutable.Set[String], dateTimeStrategies : mutable.Set[String])(implicit knowledgeGraph: KnowledgeGraph) = {
    val dataset = KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph)
    //Add core.strategies to ontology
    UpdateQueryFactory.addStatements(propertiesToPropTypeMap.map { case (key, value) => s"<$key> <${SimilarPropertyOntology.rdfType}> <${SimilarPropertyOntology.propertyMatchStrategy}> ." }, dataset)
    UpdateQueryFactory.addStatements(sameTypePossibleProps.map { (v) => s"<$v> <${SimilarPropertyOntology.rdfType}> <${SimilarPropertyOntology.alternativeLinkStrategy}> ." }, dataset)
    UpdateQueryFactory.addStatements(sameTypePossibleProps.map { (v) => s"<$v> <${SimilarPropertyOntology.rdfType}> <${SimilarPropertyOntology.directLinkStrategy}> ." }, dataset)
    UpdateQueryFactory.addStatements(sharableDomainProps.map { (prop) => s"<$prop> <${SimilarPropertyOntology.rdfType}> <${SimilarPropertyOntology.valueMatchSubjectStrategy}> ." }, dataset)
    UpdateQueryFactory.addStatements(sharableRangeProps.map { (prop) => s"<$prop> <${SimilarPropertyOntology.rdfType}> <${SimilarPropertyOntology.valueMatchObjectStrategy}> ." }, dataset)
    UpdateQueryFactory.addStatements(dateTimeStrategies.map { (prop) => s"<$prop> <${SimilarPropertyOntology.rdfType}> <${SimilarPropertyOntology.dateTimeStrategy}> ." }, dataset)
  }

  def addMetaKnowledgeToDatabase(propToDomainCount: mutable.Map[String, Int], propToRangeCount: mutable.Map[String, Int])(implicit knowledgeGraph: KnowledgeGraph) = {
    val dataset = KnowledgeGraph.findDatasetForStoringStrategiesAndMetadata(knowledgeGraph)
    //Add meta-property knowledge to ontology
    UpdateQueryFactory.addStatements(propToDomainCount.map { case (key, value) => s"""<$key> <${SimilarPropertyOntology.domainCount}> "$value" .""" }, dataset)
    UpdateQueryFactory.addStatements(propToRangeCount.map { case (key, value) => s"""<$key> <${SimilarPropertyOntology.rangeCount}> "$value" .""" }, dataset)
  }

  def findMetaPropertyKnowledge(propertiesToPropTypeMap: Map[String, PropertyType])(implicit knowledgeGraph: KnowledgeGraph) = {
    val propToDomainCount = mutable.Map[String, Int]()
    val propToRangeCount = mutable.Map[String, Int]()
    val sameTypePossibleProps = mutable.Set[String]()
    val sharableDomainProps = mutable.Set[String]()
    val sharableRangeProps = mutable.Set[String]()
    val dateProperties = mutable.Set[String]()
    var counter = 0
    for ((prop, propType) <- propertiesToPropTypeMap) {
      counter +=1
      println(s"finished $counter of ${propertiesToPropTypeMap.size}")
      propToDomainCount += prop -> QueryFactory.findDomainCount(prop)
      propType match {
        case a: ItemPropertyType => {
          propToRangeCount += (prop -> QueryFactory.findRangeCount(prop))
          if (AskQuery.sameTypePossibleForProp(prop)) sameTypePossibleProps += prop
          if (AskQuery.sharableDomain(prop)) sharableDomainProps += prop
          if (AskQuery.sharableRange(prop)) sharableRangeProps += prop
        }
        case a: DateTimePropertyType => dateProperties += prop
        case _ =>
      }

    }
    (propToDomainCount, propToRangeCount, sameTypePossibleProps, sharableDomainProps, sharableRangeProps, dateProperties)
  }
}
