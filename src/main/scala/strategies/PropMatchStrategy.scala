package strategies

import feature.Feature
import globals.FeatureType
import query.specific.QueryFactoryV2
import query.variables.{DynamicQueryVariable, OptionsForResultQueryVariable, StaticQueryVariable}
import rdf.{GraphRDF, SimpleRDF, SimpleRDFFactory}

import scala.collection.mutable
/**
  * Created by Espen on 10.11.2016.
  */
case class PropMatchStrategy(property : String, isSubject : Boolean, override  val weight: Double, rdfType : String) extends Strategy {

  def matchEntityAndProperty(tuple: (String, String, String), entity : String): Boolean = {
    return tuple match {
      case (`entity`, `property`, o) => true
      case (s, `property`, `entity`) => true
      case _ => false
    }
  }

  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = {
    val featureMap = mutable.Map[String, Feature]()
    for (other <- otherEntities) {
      val entity: String = other.entity
      if(other.statementsList.exists(matchEntityAndProperty(_, entity))) {
          featureMap += entity -> new Feature(property, FeatureType.sameProperty, 1, weight)
        }
      }
    return featureMap.toMap

  }

  override def findSimilars(): List[String] = {
    if(isSubject) return QueryFactoryV2.findList(SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.sameTypeFilter + "_" + rdfType, property, "?o " + OptionsForResultQueryVariable.ignoreMe)))
    else return QueryFactoryV2.findList(SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.ignoreMe , property, "?o " + OptionsForResultQueryVariable.sameTypeFilter + "_" + rdfType)))
  }


}
