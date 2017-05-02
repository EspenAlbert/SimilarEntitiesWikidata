package core.globals

/**
  * Created by Espen on 04.11.2016.
  */
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{DateTimePropertyType, ItemPropertyType, PropertyType, QuantityPropertyType, StringPropertyType, UrlPropertyType}
import core.query.specific.{AskQuery, QueryFactory}
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
  val xmlSchemaPattern = ".*(http://www.w3.org/2001/XMLSchema#\\w*).*".r
  val rdfLangString = ".*(http://www.w3.org/1999/02/22-rdf-syntax-ns#langString).*".r
  def getDatatypeAsStringFromResult(result : String) : Option[String] = {
    result match {
      case xmlSchemaPattern(v) => return Some(v)
      case rdfLangString(v) => return Some(v)
      case "" => return None
      case _ => println(s"Unknown datatype $result");return None
    }
  }

  def getYearFromDateFormat(stringDateRaw : String) : Option[Int] = {
    stringDateRaw.trim() match {
      case x if x endsWith("gYearMonth>") => return Some(x.substring(1, 5).toInt)
      case x if x endsWith("#date>") => return Some(x.substring(1, 5).toInt)
      case x if x endsWith("gYear>") => return Some(x.substring(1, 5).toInt)
      case x => println(s"dont know how to convert $x"); return None
    }
  }
  val prefixXsd = "http://www.w3.org/2001/XMLSchema#"
  val numericTypes = List(
    "float", "double", "decimal"
  ).map(prefixXsd + _)
  val dateTypes = List(
    "gYearMonth","date", "gYear"
  ).map(prefixXsd + _)
  val stringTypes = List(
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString",
    "http://www.w3.org/2001/XMLSchema#string"
  )
  val xmlSchemaPatternType = ".*(http://www.w3.org/2001/XMLSchema#)(\\w*).*".r


  val literalValuePattern = """".*"""".r

  def getPropertyTypeFromDatatypes(datatypes : List[String]): Option[PropertyType] = {
    if(datatypes.exists(numericTypes.contains(_))) return Some(QuantityPropertyType())
    if(datatypes.exists(stringTypes.contains(_))) return Some(StringPropertyType())
    if(datatypes.exists(dateTypes.contains(_))) return Some(DateTimePropertyType())
    else None
  }
  def determineFromObjectValuePropertyType(property : String)(implicit knowledgeGraph: KnowledgeGraph) : Option[PropertyType] = {
    val samples = QueryFactory.find100SamplesForProperty(property)
    if(samples.count(_.startsWith("http")) == samples.length) return Some(UrlPropertyType())
    if(samples.exists{case literalValuePattern(_*) => false; case _=> true}) return Some(StringPropertyType()) //IF we find a literal value not wrapped in "" it is not a string property type
    else None
  }

}
