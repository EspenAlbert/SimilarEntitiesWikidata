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
    DumpObject.dumpJsonMapStringTuple(getMap().toMap, propertiesidfscoresFilename)
  }

  def getMap() : mutable.Map[String, (Double, Double)] = {
    val properties = QueryFactory.findAllPropertiesOfCustomClass(CustomPropertyClass.baseProperty)
    val countId = SimilarPropertyOntology.spoCount.toString
    val propertyIdfScore = mutable.Map[String, (Double, Double)]()
    for(prop <- properties.filterNot(_ == "http://www.wikidata.org/entity/P31")) {
      val domainCount = QueryFactory.findDomainCount(prop)
      val idfScoreDomain = Math.log(SimilarPropertyOntology.maxCountForProperties / domainCount)
      try {
        val rangeCount = QueryFactory.findRangeCount(prop)
        val idfScoresRange = Math.log(SimilarPropertyOntology.maxCountForProperties / rangeCount)
        println(s"idf scores for : $prop = $idfScoreDomain , $idfScoresRange ")
        propertyIdfScore(prop) = (idfScoreDomain, idfScoresRange)
      } catch {
        case a : Throwable => {
          propertyIdfScore(prop) = (idfScoreDomain, idfScoreDomain)
        }
      }

    }
    return propertyIdfScore
  }
  def getMapFromFile() : Map[String, (Double, Double)] = {
    return DumpObject.getMapStringTuple(propertiesidfscoresFilename)
  }

}
