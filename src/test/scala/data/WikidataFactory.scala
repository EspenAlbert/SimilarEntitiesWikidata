package data

import core.globals.{KnowledgeGraph, MyDatasets, SimilarPropertyOntology}
import core.rdf.GraphRDF

/**
  * Created by espen on 16.03.17.
  */
object WikidataFactory {
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val directLinkProperty = "http://www.wikidata.org/entity/P1393"
  val itemProperty = "http://www.wikidata.org/entity/P1026"
  val valueMatchObject = "http://www.wikidata.org/entity/P1072"
  val dateProperty = "http://www.wikidata.org/entity/P575"
  val quantityProp = "http://www.wikidata.org/entity/P1971"
  val itemProperties = List(directLinkProperty, valueMatchObject, itemProperty)

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

  val obama = "http://www.wikidata.org/entity/Q76"
  val human = "http://www.wikidata.org/entity/Q5"
  def obamaSubjectStatements: List[(String, String, String)] = {
    val p1o = List("http://www.wikidata.org/entity/Q380782", "http://www.wikidata.org/entity/Q1379733")
    val p = "http://www.wikidata.org/entity/P410" //Military Rank
    val p2 = "http://www.wikidata.org/entity/P106"
    val p2o = List("http://www.wikidata.org/entity/Q82955"," http://www.wikidata.org/entity/Q40348"," http://www.wikidata.org/entity/Q15958642")
    return p1o.map(o => (obama, p, o)) ++ p2o.map(o => (obama, p2, o))
  }
  def obamaObjectStatements: List[(String, String, String)] = {
    val p1 = "http://www.wikidata.org/entity/P180" //depiction of, painting of, motif, portrait of, landscape of, represents
    val p1s = List("http://www.wikidata.org/entity/Q2915674"," http://www.wikidata.org/entity/Q4858105"," http://www.wikidata.org/entity/Q5842038")
    val p2 = "http://www.wikidata.org/entity/P26"//spouse
    val p2s = "http://www.wikidata.org/entity/Q13133"
    return List((p2s, p2, obama)) ++ p1s.map(s => (s, p1, obama))
  }
  def obamaStatements: List[(String, String, String)] = {
    return obamaSubjectStatements ++ obamaObjectStatements
  }

  def kristiansandAndTypes(): (GraphRDF, List[String]) = {
    val kristiansand = new GraphRDF(SimilarPropertyOntology.w + "Q2415")
    val type1 = "http://www.wikidata.org/entity/Q515"
    val type2 = "http://www.wikidata.org/entity/Q755707"
    val expectedTypes = List(type1, type2)
    return (kristiansand, expectedTypes )
  }
}
