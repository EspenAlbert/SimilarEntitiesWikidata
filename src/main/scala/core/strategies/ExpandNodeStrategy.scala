package core.strategies

import core.feature.{ExpandNodeFeature, Feature}
import core.globals.{FeatureType, KnowledgeGraph}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.QueryFactory
import core.rdf.GraphRDF
import similarityFinder.MyConfiguration

/**
  * Created by espen on 18.04.17.
  */
case class ExpandNodeStrategy (property : String, values : List[String], types: List[String], isSubject : Boolean = true, qEntity: String = "notDefined") extends Strategy {
  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    return findSimilars()
  }

  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
    val prefix = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)
    val l1Values = filterOnlyEntities(prefix, values)
    val length1Entities = l1Values.map(e => e -> createFeatureLength1(e)).toMap
    //Length 2, middle entity is object
    val l2PropertiesAndSubjects =
      for {
        entity <- l1Values
        propertiesWhereObject = QueryFactory.findLowCountPropertiesWhereEntityIsObject(entity)
        prop <- propertiesWhereObject
//        if !StrategyFactory.isDescriptive(prop) || StrategyFactory.valueIsAPotentialValueMatchFindCount(entity, prop, false).get < MyConfiguration.thresholdCountCheapStrategy
        otherEntities = findSubjectsWhereEntityIsObjectForProperty(entity, prop)
      } yield (entity,prop,otherEntities)
    val l2Subjects = l2PropertiesAndSubjects.map{
      case (middleNode,prop, otherEntities) => otherEntities.
        filter(_.startsWith(prefix)).
        map(e => e -> createFeatureLength2(e, prop,middleNode, false))
    }.flatten.toMap
    //Length 2, middle entity is subject
    val l2ValuesObjects = l1Values
      .map(middleEntity => (middleEntity, QueryFactory.findPropertiesAndObjects(middleEntity)))
      .flatMap {
        case (middleEntity, (propertyList, objectList)) => objectList
          .zipWithIndex.
          map {
            case (foundEntity, index) if foundEntity.startsWith(prefix) =>
              Some(foundEntity -> createFeatureLength2(foundEntity, propertyList(index), middleEntity, true))
            case _ => None
          }
      }.flatten.toMap

//    if(MyConfiguration.filterOnRdfType) return (StrategyFactory.getDomainAndRangeWithCorrectType(l1Values.keys.toList, Nil, types)._1 ++ l2ValuesObjects ++ l2PropertiesAndSubjects).map(e => e ->feature).toMap
    return length1Entities ++ l2ValuesObjects ++ l2Subjects
  }

  private def findSubjectsWhereEntityIsObjectForProperty(entity: String, property : String)(implicit knowledgeGraph: KnowledgeGraph) = {
    MyConfiguration.filterOnRdfType match {
      case false => QueryFactory.subjectsWithPropertyAndValue(property, entity)
      case true => {
        QueryFactory.subjectsOfTypeWithPropertyAndValue(property, entity, types)
      }
    }
  }

  private def filterOnlyEntities(prefix: String, values : Iterable[String]): Iterable[String] = {
    return values.filter(_.startsWith(prefix))
  }

  def createFeatureLength1(foundEntity : String): Feature = {
    val path: String = pathLength1(foundEntity)
    return new ExpandNodeFeature(property,path)
  }

  private def pathLength1(foundEntity: String) : String= {
    if (isSubject) s"$qEntity --> $property --> $foundEntity" else s"$qEntity <-- $property <-- $foundEntity"
  }

  def createFeatureLength2(foundEntity : String, secondProperty : String, middleNode: String, middleNodeIsSubject : Boolean): Feature = {
    val startPath = pathLength1(middleNode)
    val secondPath = if(middleNodeIsSubject) s" --> $secondProperty --> $foundEntity" else s" <-- $secondProperty <-- $foundEntity"
    return new ExpandNodeFeature(property, startPath+secondPath)
  }

  override val name: String = ExpandNodeStrategy.name
}

object ExpandNodeStrategy {
  val name: String = "ExpandNodeStrategy"
  var mustHaveProperty : String = ""
  var mustHavePropertyIsSubject : Boolean = true
}
