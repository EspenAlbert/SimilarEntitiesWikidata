package query.variables

import globals.{SimilarPropertyOntology, PrimitiveDatatype}
import globals.PrimitiveDatatype.PrimitiveDatatype

/**
  * Created by Espen on 07.11.2016.
  */
class StaticQueryVariable(val name : String, val datatype: PrimitiveDatatype = null) extends QueryVariable {

  private def getUriValue(value: String) : String = {
    if(value.startsWith("http:")) return value
    if(value.contains(":")) {
      val splitIndex: Int = value.indexOf(":")
      val (prefix, id) = (value.substring(0, splitIndex), value.substring(splitIndex + 1))
      val fullUri = SimilarPropertyOntology.getFromRawString(prefix) + id
      return fullUri
    }
    return PrimitiveDatatype.getDatatypeValue(name, datatype)
  }
  def isAllDigits(x: String) = x forall Character.isDigit

  def convertToSparql(value: String): String = {
    val uri: String = getUriValue(value)
    if(uri.startsWith("\"")) return uri
    return "<" + uri + ">"
  }

  override def getSelectPhrase: String = {
    return ""
  }

  override def getWherePhrase: String = {
    convertToSparql(name)
  }
  def getNtValue() : String = {
    return convertToSparql(name)
  }
}
