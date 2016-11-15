package globals

/**
  * Created by Espen on 04.11.2016.
  */
object SimilarPropertyOntology extends Enumeration{


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
  val basePropertyClassId = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#Property")
  val maxCountForProperties = Value("17871093")
  val maxPropertyNumber = Value("3333")
  def getFromRawString(value : String) : SimilarPropertyOntology = {
    value match {
      case "w" => return w
      case "rdfType" => return rdfType
      case "spo" => return spo
      case a : String => throw new Exception("tried to get a namespace value from raw string" + value)
    }
    return null
  }
}
