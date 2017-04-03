package core.strategies

import core.feature.Feature
import core.globals.FeatureType
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.QueryFactory
import core.rdf.GraphRDF
import similarityFinder.MyConfiguration

import scala.collection.mutable
/**
  * Created by Espen on 10.11.2016.
  */
case class PropertyMatchStrategy(property : String, isSubject : Boolean, rdfTypes : List[String], dbCount: Int) extends Strategy {

  def matchEntityAndProperty(tuple: (String, String, String), entity : String): Boolean = {
    return tuple match {
      case (`entity`, `property`, o) if(isSubject) => true
      case (s, `property`, `entity`) if(!isSubject)=> true
      case _ => false
    }
  }

  override def execute(otherEntities: List[GraphRDF])(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
//    println(s"Finding similars for property (expensive strategy): $property")
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      if(other.statementsList.exists(matchEntityAndProperty(_, entity))) {
          featureMap += entity -> new Feature(property, FeatureType.sameProperty, 1, weight)
        }
      }
    return featureMap.toMap
  }


  override def findSimilars()(implicit knowledgeGraph: KnowledgeGraph): Map[String, Feature] = {
//    println(s"Finding similars for property : $property")
    val entities = MyConfiguration.useRdfType match{
      case true => if(isSubject) QueryFactory.findSubjectsOfTypeForProperty(property, rdfTypes)
      else QueryFactory.findObjectsOfTypeForProperty(property, rdfTypes)
      case false => if(isSubject) QueryFactory.findSubjectsOfProperty(property) else QueryFactory.findObjectsOfProperty(property)
    }
    return generateFeatures(entities)
  }
  private def generateFeatures(entities: List[String]): Map[String, Feature] = {
    return entities.map(e => e -> new Feature(property, FeatureType.sameProperty, 1, weight)).toMap
  }
  val name = PropertyMatchStrategy.name
  override def toString: String = {
    s"$name for : $property isSubject=$isSubject, weight=$weight" + super.toString
  }

}

object PropertyMatchStrategy {
  val name = "PropMatchStrategy"

}
