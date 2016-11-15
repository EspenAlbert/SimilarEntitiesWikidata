package baseline

import dump.DumpObject
import globals.{MyDatasets, SimilarPropertyOntology}
import ownOntologyPopularizer.CustomPropertyClass
import query.specific.QueryFactory
import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.SimpleRDF

import scala.collection.mutable


/**
  * Created by Espen on 07.11.2016.
  */
object PropertyIdfCalculator {
  final val propertiesidfscoresFilename: String = "propertiesIdfScores"
  def dumpToFile() = {
    DumpObject.dumpJsonMap(getMap().toMap, propertiesidfscoresFilename)
  }

  def getMap() : mutable.Map[String, Double] = {
    val properties = QueryFactory.findAllPropertiesOfCustomClass(CustomPropertyClass.baseProperty)
    QueryFactory.dataset = MyDatasets.SimilarProperties
    val countId = SimilarPropertyOntology.spoCount.toString
    val propertyIdfScore = mutable.Map[String, Double]()
    for(prop <- properties) {
      val resultVariable: DynamicQueryVariable = new DynamicQueryVariable("o", false)
      val countForProperty = QueryFactory.findIntVariableValue(new SimpleRDF(new StaticQueryVariable(prop), new StaticQueryVariable(countId), resultVariable), resultVariable)
      val idfScore = Math.log(SimilarPropertyOntology.maxCountForProperties.toString.toInt / countForProperty)
      println(s"idf score for : $prop = $idfScore ")
      propertyIdfScore(prop) = idfScore
    }
    return propertyIdfScore
  }
  def getMapFromFile() : Map[String, Double] = {
    return DumpObject.loadJsonMap(propertiesidfscoresFilename)
  }

}
