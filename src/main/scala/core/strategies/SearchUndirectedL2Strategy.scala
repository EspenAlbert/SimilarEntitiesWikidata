package core.strategies

import core.feature.{Feature, PathFeature}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{FeatureType, KnowledgeGraph}
import core.query.specific.QueryFactory
import core.rdf.GraphRDF

/**
  * Created by espen on 31.03.17.
  */
case class SearchUndirectedL2Strategy(property : String, values : List[String], qEntity : String = "notDefined", isSubject : Boolean = true) extends Strategy{
  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return findSimilars()
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val prefix = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    val l1Values = filterOnlyEntities(prefix, values)
    .map(e => e -> new PathFeature(property, PathFeature.createPathLength1(qEntity, property, isSubject, e), FeatureType.searchUndirectedL2)).toMap
    val l2ValuesObjects = findNextLevel(l1Values, prefix, findObjects = true)
    val l2ValuesSubjects = findNextLevel(l1Values, prefix, findObjects = false)
    return l1Values ++ l2ValuesObjects ++ l2ValuesSubjects
  }

  private def findNextLevel(l1Values: Map[String, PathFeature], prefix : String, findObjects : Boolean)(implicit knowledgeGraph: KnowledgeGraph) = {
    l1Values.keys
      .map(middleEntity => (middleEntity, if(findObjects) QueryFactory.findPropertiesAndObjects(middleEntity) else QueryFactory.findSubjectsAndProperties(middleEntity)))
      .flatMap {
        case (middleEntity, (entityList, propertyList)) => entityList
          .zipWithIndex.
          map {
            case (foundEntity, index) if foundEntity.startsWith(prefix) =>
              Some(foundEntity -> new PathFeature(property, PathFeature.createPathLength2(qEntity, isSubject, property, middleEntity, middleNodeIsSubj = findObjects, property2 = propertyList(index), foundEntity = foundEntity), FeatureType.searchUndirectedL2))
            case _ => None
          }
      }.flatten.toMap
  }

  private def filterOnlyEntities(prefix: String, values : Iterable[String]): Iterable[String] = {
    return values.filter(_.startsWith(prefix))
  }

  override val name: String = SearchUndirectedL2Strategy.name
}

object SearchUndirectedL2Strategy {
  val name: String = "SearchUndirectedL2Strategy"
}






