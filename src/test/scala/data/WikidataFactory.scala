package data

import core.globals.{KnowledgeGraph, MyDatasets, SimilarPropertyOntology}
import core.rdf.GraphRDF

/**
  * Created by espen on 16.03.17.
  */
object WikidataFactory {
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val directLinkProperty = "http://www.wikidata.org/entity/P1393"
  val valueMatchSubject = "http://www.wikidata.org/entity/P1026"
  val valueMatchObject = "http://www.wikidata.org/entity/P1072"
  val dateProperty = "http://www.wikidata.org/entity/P575"
  val quantityProp = "http://www.wikidata.org/entity/P1971"
  val itemProperties = List(directLinkProperty, valueMatchObject, valueMatchSubject)

  val externalIdProperties : List[String]= List("http://www.wikidata.org/entity/P2099", "http://www.wikidata.org/entity/P2736", "http://www.wikidata.org/entity/P597", "http://www.wikidata.org/entity/P2642", "http://www.wikidata.org/entity/P3192")
  val stringProperties = flattenList(List("http://www.wikidata.org/entity/P645", externalIdProperties)).asInstanceOf[List[String]]
  val urlProperties = List("http://www.wikidata.org/entity/P2488")
  val ordinaryProperties : List[String] = flattenList(List(urlProperties, stringProperties, itemProperties, dateProperty, quantityProp)).asInstanceOf[List[String]]


  val qualifierProperties = List("http://www.wikidata.org/entity/P39P1734q", "http://www.wikidata.org/entity/P18P580q")
  val qualifierGeoProperties = List("http://www.wikidata.org/entity/P20P625qla", "http://www.wikidata.org/entity/P20P625qlo", "http://www.wikidata.org/entity/P885P625qla", "http://www.wikidata.org/entity/P885P625qlo")
  val geoProperties = List("http://www.wikidata.org/entity/P625lo", "http://www.wikidata.org/entity/P625la", "http://www.wikidata.org/entity/P1333la", "http://www.wikidata.org/entity/P1333lo")

  val allProperties : List[String] = flattenList(List(
    ordinaryProperties,
    qualifierProperties,
    qualifierGeoProperties,
    geoProperties)).asInstanceOf[List[String]]

  def flattenList(ls : List[Any]): List[Any] = {
    ls.flatMap{
      case i: List[_] => flattenList(i)
      case e => List(e)
    }
  }

  def kristiansandAndTypes(): (GraphRDF, List[String]) = {
    val kristiansand = new GraphRDF(SimilarPropertyOntology.w + "Q2415")
    val type1 = "http://www.wikidata.org/entity/Q515"
    val type2 = "http://www.wikidata.org/entity/Q755707"
    val expectedTypes = List(type1, type2)
    return (kristiansand, expectedTypes )
  }
}
