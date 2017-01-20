package globals

/**
  * Created by Espen on 04.11.2016.
  */
object SimilarPropertyOntology extends Enumeration{
  val maxCountForProperties = 17871093
  val maxPropertyNumber = 3333


  type SimilarPropertyOntology = Value
  val w = Value("http://www.wikidata.org/entity/")
  val rdfType = Value("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  val rdfsSubclassOf = Value("http://www.w3.org/2000/01/rdf-schema#subClassOf")
  val spoBaseStrategy = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#Strategy")
  val spo = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology")
  val spoCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#count")
  val domainCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#domainCount")
  val rangeCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#rangeCount")
  val sharableDomain: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#sharableDomain")
  val sharableRange: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#sharableRange")
  val sameTypePossible: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#sameTypePossible")
  val valueMatchClass = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatch")
  val maxMinStats = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#MaxMinStats")
  val valueMatchProperty = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchProperty")
  val valueMatchValue = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchValue")
  val valueMatchCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchCount")
  val valueMatchSubjectStrategy = Value(spo + "#ValueMatchSubjectStrategy")
  val valueMatchObjectStrategy = Value(spo + "#ValueMatchObjectStrategy")
  val alternativeLinkStrategy = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy")
  val directLinkStrategy = Value(spo + "#DirectLinkStrategy")

  val basePropertyClassId = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#Property")
  def getFromRawString(value : String) : SimilarPropertyOntology = {
    value match {
      case "w" => return w
      case "rdfType" => return rdfType
      case "spo" => return spo
      case a : String => throw new Exception("tried to get a namespace value from raw string" + value)
    }
    return null
  }
  implicit def getStringFromOptionsForResultQueryVariable(similarPropertyOntology: SimilarPropertyOntology) : String = {
    return similarPropertyOntology.toString
  }


}
