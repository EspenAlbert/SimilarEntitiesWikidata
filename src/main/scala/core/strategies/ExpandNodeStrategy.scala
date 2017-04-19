package core.strategies

import core.feature.Feature
import core.globals.{FeatureType, KnowledgeGraph}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.QueryFactory
import core.rdf.GraphRDF
import similarityFinder.MyConfiguration

/**
  * Created by espen on 18.04.17.
  */
case class ExpandNodeStrategy (property : String, values : List[String], types: List[String]) extends Strategy {
  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return findSimilars()
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val prefix = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    val l1Values = filterOnlyEntities(prefix, values)
    val l2Subjects = {
      for {
        entity <- l1Values.keys
        propertiesWhereObject = QueryFactory.findDistinctPropertiesWhereObject(entity)
        prop <- propertiesWhereObject
        if (!StrategyFactory.isDescriptive(prop) || StrategyFactory.valueIsAPotentialValueMatchFindCount(entity, prop, false).get < MyConfiguration.thresholdCountCheapStrategy)
        otherEntities = findSubjectsWhereEntityIsObjectForProperty(entity, prop)
      } yield otherEntities
    }.flatten
    val l2ValuesObjects = l1Values.keys.flatMap(QueryFactory.findPropertiesAndObjects(_)._1)
    if(MyConfiguration.filterOnRdfType) return (StrategyFactory.getDomainAndRangeWithCorrectType(l1Values.keys.toList, Nil, types)._1 ++ l2ValuesObjects ++ l2Subjects).map(e => e ->feature).toMap
    return l1Values ++ filterOnlyEntities(prefix, l2ValuesObjects) ++ filterOnlyEntities(prefix, l2Subjects)
  }

  private def findSubjectsWhereEntityIsObjectForProperty(entity: String, property : String)(implicit knowledgeGraph: KnowledgeGraph) = {
    MyConfiguration.filterOnRdfType match {
      case false=> QueryFactory.subjectsWithPropertyAndValue(property, entity)
      case true => {
        QueryFactory.subjectsOfTypeWithPropertyAndValue(property, entity, types)
      }
    }
  }
  private def findObjectsWhereEntityIsSubjectForProperty(entity: String, property : String)(implicit knowledgeGraph: KnowledgeGraph) = {
    MyConfiguration.filterOnRdfType match {
      case false=> QueryFactory.objectsWithPropertyAndSubject(property, entity)
      case true => {
        QueryFactory.objectsOfTypeWithPropertyAndSubject(property, entity, types)
      }
    }
  }

  private def filterOnlyEntities(prefix: String, values : Iterable[String]): Map[String, Feature] = {
    return values.filter(_.startsWith(prefix))
      .map(e => (e -> feature))
      .toMap
  }

  val feature = new Feature(property, FeatureType.searchExpandNode, 1, weight)

  override val name: String = ExpandNodeStrategy.name
}

object ExpandNodeStrategy {
  val name: String = "ExpandNodeStrategy"
  var mustHaveProperty : String = ""
  var mustHavePropertyIsSubject : Boolean = true
}
