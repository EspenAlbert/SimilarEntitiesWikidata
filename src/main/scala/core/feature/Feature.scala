package core.feature

import java.io.ByteArrayOutputStream

import breeze.numerics.floor
import core.globals.FeatureType.FeatureType
import core.query.QueryForOnlineWikidata
import jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 09.11.2016.
  */
class Feature(property : String, featureType : FeatureType, count : Int, weight : Double) extends Ordered[Feature]{
  def getScore() : Double = {
    return count * weight
  }

  override def compare(that: Feature): Int = {
    val compared = that.getScore() - this.getScore()
    if(compared > 0) return 1 //Other has higher score
    if(compared < 0) return -1 //This has higher score
    return 0
  }

  override def toString: String = {
    val label = Feature.findLabel(property)
    println(label)
    return s"Feature for $property with label: $label with $featureType had value: $count * $weight"
  }


}

object Feature {
  def findLabel(id : String) : String = {
    val commonPrefixes = "PREFIX wd: <http://www.wikidata.org/entity/>\nPREFIX wdt: <http://www.wikidata.org/prop/direct/>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
    val labelQuery = new QueryForOnlineWikidata(() => commonPrefixes + s"select ?label \n where { <$id> rdfs:label ?label . \n filter(lang(?label) = 'en')\n }")
    labelQuery.executeRaw()
    val label = labelQuery.getResults("label")(0)
    label
  }
}
