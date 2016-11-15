package baseline

import dump.DumpObject
import globals.SimilarPropertyOntology
import ownOntologyPopularizer.CustomPropertyClass
import query.specific.{QueryFactory, QueryFactoryV2}
import query.variables.DynamicQueryVariable
import rdf.SimpleRDFFactory

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
    val countId = SimilarPropertyOntology.spoCount.toString
    val propertyIdfScore = mutable.Map[String, Double]()
    for(prop <- properties) {
      val countForProperty = QueryFactoryV2.findSingleValue(SimpleRDFFactory.getStatement(prop, countId, "?o"))
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
