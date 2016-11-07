package globals

/**
  * Created by Espen on 04.11.2016.
  */
object Namespace extends Enumeration{

  type Namespace = Value
  val w = Value("http://www.wikidata.org/entity/")
  val rdfType = Value("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  val spo = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology")
  def getFromRawString(value : String) : Namespace = {
    value match {
      case "w" => return w
      case "rdfType" => return rdfType
      case "spo" => return spo
      case a : String => throw new Exception("tried to get a namespace value from raw string")
    }
    return null
  }
}