package data

/**
  * Created by espen on 16.03.17.
  */
object WikidataFactory {
  val ordinaryProperties = List("http://www.wikidata.org/entity/P2099", "http://www.wikidata.org/entity/P2736", "http://www.wikidata.org/entity/P597", "http://www.wikidata.org/entity/P2642", "http://www.wikidata.org/entity/P3192", "http://www.wikidata.org/entity/P645", "http://www.wikidata.org/entity/P639")
  val qualifierProperties = List("http://www.wikidata.org/entity/P39P1734q", "http://www.wikidata.org/entity/P18P580q")
  val qualifierGeoProperties = List("http://www.wikidata.org/entity/P20P625qla", "http://www.wikidata.org/entity/P20P625qlo", "http://www.wikidata.org/entity/P885P625qla", "http://www.wikidata.org/entity/P885P625qlo")
  val geoProperties = List("http://www.wikidata.org/entity/P625lo", "http://www.wikidata.org/entity/P625la", "http://www.wikidata.org/entity/P1333la", "http://www.wikidata.org/entity/P1333lo")
  val allProperties = List(
    ordinaryProperties,
    qualifierProperties,
    qualifierGeoProperties,
    geoProperties).flatten
}
