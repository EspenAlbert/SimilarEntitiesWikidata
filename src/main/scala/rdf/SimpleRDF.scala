package rdf

/**
  * Created by Espen on 04.11.2016.
  */
import globals.Namespace
class SimpleRDF(val s: String, val p: String, val o: String) {
  def sInSparql = convertToSparql(s)
  def pInSparql = convertToSparql(p)
  def oInSparql = convertToSparql(o)

  private def convertToSparql(value: String): String = {
    val uri: String = getUriValue(value)
    if(uri.startsWith("\"")) return uri
    return "<" + uri + ">"
  }
  private def getUriValue(value: String) : String = {
    if(value.startsWith("http:")) return value
    if(value.contains(":")) {
      val splitIndex: Int = value.indexOf(":")
      val (prefix, id) = (value.substring(0, splitIndex), value.substring(splitIndex + 1))
      val fullUri = Namespace.getFromRawString(prefix) + id
      return fullUri
    }
    return "\"" + value + "\""//Most likely a datatype
  }
  def isAllDigits(x: String) = x forall Character.isDigit

  def getStatementNt(): String = {
    var subject: String = sInSparql
    var predicate: String = pInSparql
    var objectValue = oInSparql
    if(predicate.endsWith("c>")) {
      predicate =  pInSparql.dropRight(2) + ">"
    }
    if(isAllDigits(objectValue)) return subject + " " + predicate + " "+ objectValue
    return subject + " " + predicate + " "+ objectValue
  }

}