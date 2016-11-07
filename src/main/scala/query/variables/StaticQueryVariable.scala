package query.variables

import globals.Namespace
import globals.PrimitiveDatatype.PrimitiveDatatype

/**
  * Created by Espen on 07.11.2016.
  */
//TODO: Check how to handle different value types...
class StaticQueryVariable(val name : String, val datatype: PrimitiveDatatype = null) extends QueryVariable {

  private def getUriValue(value: String) : String = {
    if(value.startsWith("http:")) return value
    if(value.contains(":")) {
      val splitIndex: Int = value.indexOf(":")
      val (prefix, id) = (value.substring(0, splitIndex), value.substring(splitIndex + 1))
      val fullUri = Namespace.getFromRawString(prefix) + id
      return fullUri
    }
//    datatype match {
    //      case s : PrimitiveDatatype.uint =>
    //    }
    return "\"" + value + "\""//Most likely a datatype
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
