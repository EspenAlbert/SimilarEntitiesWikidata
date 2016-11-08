package baseline

import globals.{MyDatasets, Namespace}
import query.specific.QueryFactory
import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.SimpleRDF

import scala.collection.mutable


/**
  * Created by Espen on 07.11.2016.
  */
object PropertyIdfCalculator {
  def getMap() : mutable.Map[String, Double] = {
    val properties = QueryFactory.findAllPropertiesOfCustomClass()
      QueryFactory.dataset = MyDatasets.SimilarProperties
    val countId = QueryFactory.findIDForPropertyLabelQuery("count")
    val propertyIdfScore = mutable.Map[String, Double]()
    for(prop <- properties) {
      val resultVariable: DynamicQueryVariable = new DynamicQueryVariable("o", false)
      val countForProperty = QueryFactory.findIntVariableValue(new SimpleRDF(new StaticQueryVariable(prop), new StaticQueryVariable(countId), resultVariable), resultVariable)
      val idfScore = Math.log(Namespace.maxCountForProperties.toString.toInt / countForProperty)
      println(s"idf score for : $prop = $idfScore ")
      propertyIdfScore(prop) = idfScore
    }
    return propertyIdfScore

  }

}
