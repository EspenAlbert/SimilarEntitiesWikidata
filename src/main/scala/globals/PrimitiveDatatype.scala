package globals

/**
  * Created by Espen on 04.11.2016.
  */
object PrimitiveDatatype extends Enumeration{


  type PrimitiveDatatype = Value
  val nonNegativeInteger: PrimitiveDatatype = Value("http://www.w3.org/2001/XMLSchema#nonNegativeInteger")
  val boolean: PrimitiveDatatype = Value("http://www.w3.org/2001/XMLSchema#boolean")
  val string : PrimitiveDatatype = Value("http://www.w3.org/2001/XMLSchema#string")

  def getDatatypeValue(value : String, datatype : PrimitiveDatatype) : String = {
    return datatype match {
      case PrimitiveDatatype.nonNegativeInteger => getProperStatementSyntax(value, nonNegativeInteger)
      case PrimitiveDatatype.boolean => getProperStatementSyntax(value, boolean)
      case PrimitiveDatatype.string => getProperStatementSyntax(value, string)
      case _ => "\"" + value + "\""
    }
  }
  private def getProperStatementSyntax(value : String, d : PrimitiveDatatype) : String = {
    return "\"%s\"^^<%s>".format(value, d)
  }

}
