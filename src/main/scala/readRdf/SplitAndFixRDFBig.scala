package readRdf

import java.io.{BufferedInputStream, FileInputStream, InputStream, PrintWriter}
import java.util.zip.GZIPInputStream

import readRdf.SplitAndFixRDFBig.CustomOriginalPropertyTypes.CustomOriginalPropertyTypes

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex.MatchIterator

/**
  * Created by Espen on 13.10.2016.
  */
object SplitAndFixRDFBig {
  val NON_FIXABLE_URI = "ERROR"
  final val StatementsPerFile: Int = 100000
  val totalPattern = ("<http://[^>]*(" +
    "(\\\\\")|" +
    "(\\^)|" +
    "(\\|)|" +
    "(\\})|" +
    "(\\`)|" +
    "(\\{)|" +
    "(\\\\n)|" +
    "(\\\\\\\\)|" +
    "(\\s))" +
    "").r
  val bracketsPattern = "(\\{[^>]*\\})".r
  val subPattern = "(\\\\\")|(\\^)|(\\|)|(\\})|(\\`)|(\\{)|(\\\\n)|(\\{[^>]*\\})|(\\\\\\\\)|(\\s)".r
  val propertyEndWithCPattern = "<http://www.wikidata.org/entity/P\\d{1,4}[^>]*>".r

  var stringBuffer = new StringBuilder()
  var counter = 0
  var fileNumber = 1

  def decodeLine(line: String) = {
    findLineType(line) match {
      case l : NewEntityLine => addStatements()
      case l : EntityLine => statementIdMap += (l.statementID -> (l.entity, l.property))
      case l : StatementLine => statementIdToValuesMap.get(l.statementId) match {
        case Some(tuples) => tuples += Tuple2(l.property, l.value)
        case None => statementIdToValuesMap += (l.statementId -> mutable.ListBuffer((l.property, l.value)))
      }
      case l : ValueNodeLine => valueNodeToValuesMap.get(l.valueId) match {
        case Some(tuples) => tuples += Tuple2(l.property, l.value)
        case None => valueNodeToValuesMap += (l.valueId -> mutable.ListBuffer((l.property, l.value)))
      }
      case l : UnknownLine =>
    }
  }
  abstract class RDFLine
  case class NewEntityLine(entity : String, property : String, statementID: String) extends RDFLine
  case class EntityLine(entity : String, property : String, statementID: String) extends RDFLine
  case class StatementLine(statementId : String, property : String, value: String) extends RDFLine
  case class ValueNodeLine(valueId: String, property : String, value: String) extends RDFLine
  case class UnknownLine() extends RDFLine

  val entityPattern = """<http://www.wikidata.org/entity/Q\d+>""".r
  val statementIdPattern = """<http://www.wikidata.org/entity/Q\d+.*>""".r
  val valueNodePattern = """<http://www.wikidata.org/entity/V.*>""".r

  def findLineType(line : String) : RDFLine = {
    val Array(subj,predicate,objectValue,_) = line.split(" ")
    println(subj, predicate, objectValue)
    subj match {
      case entityPattern(_*) => objectValue match{
        case "<http://www.wikidata.org/ontology#Item>" => return NewEntityLine(subj, predicate, objectValue)
        case other => return EntityLine(subj, predicate.dropRight(2) + ">", objectValue)
      }
      case statementIdPattern(_*) => return StatementLine(subj, predicate, objectValue)
      case valueNodePattern(_*) => return ValueNodeLine(subj, predicate, objectValue)
      case _ => UnknownLine()
    }
  }


  val statementIdMap = mutable.Map[String, (String, String)]()
  val statementIdToValuesMap = mutable.Map[String, mutable.ListBuffer[(String, String)]]()
  val valueNodeToValuesMap = mutable.Map[String, mutable.ListBuffer[(String, String)]]()

  val preferredRank = "<http://www.wikidata.org/ontology#PreferredRank>"
  def addStatements() = {
    val literalMap = getLiteralMap
    val statementsAndRanks = findStatementsAndRanks(literalMap)
    val propertiesWithPrefferedRank = statementsAndRanks.filter(_._2 == preferredRank).map(_._1._2).toSet
    val finalStatements = ListBuffer[String]()
    for(((s,p,o), rank) <- statementsAndRanks) {
      if(!propertiesWithPrefferedRank.contains(p)) finalStatements.append(createStatementNt(s, p, o))
      else {
        statementIdPattern.findFirstIn(o) match {
          case Some(objectValue) => finalStatements.append(createStatementNt(s, p, o))
          case None => if(rank == preferredRank) finalStatements.append(createStatementNt(s, p, o))
        }
      }
    }
    writeStatementsToFile(finalStatements)
    statementIdMap.clear()
    statementIdToValuesMap.clear()
    valueNodeToValuesMap.clear()
  }
  private def writeStatementsToFile(s : Iterable[String]) = {
    val printWriter = new PrintWriter("input/smallFiles/wikidata-statements" + fileNumber + ".nt")
    fileNumber += 1
  }

  private def createStatementNt(s: String, p: String, o: String) : String= {
    List(s, p, o).mkString(" ") + " ."
  }

  object CustomOriginalPropertyTypes extends Enumeration{
    type CustomOriginalPropertyTypes = Value
    val statement = Value("statement")
    val qualifier = Value("qualifier")
    val latitude = Value("latitude")
    val longitude = Value("longitude")
  }
  val propertyValue = """http://www.wikidata.org/entity/P\d+v>""".r
  val qualifierValue = """http://www.wikidata.org/entity/P\d+q>""".r


  def findStatementsAndRanks(literalMap: Map[String, String]) : List[((String, String, String), String)]= {
    val statementAndRanks = ListBuffer[((String, String, String), String)]()
    for ((statementID, tupleList) <- statementIdToValuesMap) {
      var rank: String = null
      var statements =  ListBuffer[(String, String, String)]()
      def createStatement(literalValue: String, changedOriginalProperty: CustomOriginalPropertyTypes = CustomOriginalPropertyTypes.statement, qualifierProperty: String = null): (String, String, String) = {
        statementIdMap.get(statementID) match {
          case Some((subj, property)) => return (subj, getCustomOriginalProperty(CustomOriginalPropertyTypes.statement, qualifierProperty), literalValue)
          case None => println(s"A statementID not included in entity lines! $statementID"); return null;
        }
      }

      def getCustomOriginalProperty(customType: CustomOriginalPropertyTypes, qualifierProperty: String = null): String = {
        try {
          customType match {
            case CustomOriginalPropertyTypes.statement => statementIdMap.get(statementID) match {
              case Some((prop, obj)) => return prop.dropRight(2) + ">"
            }
            case CustomOriginalPropertyTypes.latitude => statementIdMap.get(statementID) match {
              case Some((prop, obj)) => return prop.dropRight(2) + "la>"
            }
            case CustomOriginalPropertyTypes.longitude => statementIdMap.get(statementID) match {
              case Some((prop, obj)) => return prop.dropRight(2) + "la>"
            }
            case CustomOriginalPropertyTypes.qualifier => statementIdMap.get(statementID) match {
              case Some((prop, obj)) => return prop.dropRight(2) + qualifierProperty + ">"
            }
          }
        }
        catch {
          case a: MatchError => println("couldn't find property or you tried a custom orginial property value that don't exists!"); "UNKOWN"
        }
      }

      for ((prop, value) <- tupleList) {
        def getStatementFromValueNode(qualifier: Boolean = false) = {
          val qualifierPropertyId = if (qualifier) prop.substring(prop.indexOf('P'), prop.length - 1) else null
          literalMap.get(value) match {
            case Some(literalValue) => statements.append(if (qualifier) createStatement(literalValue, CustomOriginalPropertyTypes.qualifier, qualifierPropertyId) else createStatement(literalValue))
            case None => (literalMap.get(value + "la"), literalMap.get(value + "lo")) match {
              //Means we have a coordinate statement
              case (Some(la), Some(lo)) =>
                if (!qualifier) {
                  statements.append(createStatement(la, CustomOriginalPropertyTypes.latitude), createStatement(lo, CustomOriginalPropertyTypes.longitude))
                }
                else {
                  statements.append(createStatement(la, CustomOriginalPropertyTypes.qualifier, qualifierPropertyId + "la"),
                createStatement(lo, CustomOriginalPropertyTypes.qualifier, qualifierPropertyId + "lo"))
                }
            }
          }
        }

        prop match {
          case "<http://www.wikidata.org/ontology#rank>" => rank = value
          case propertyValue(_*) => {
            value match {
              case valueNodePattern(_*) => {
                getStatementFromValueNode()
              }
            }
          }
          case qualifierValue(_*) => {
            value match {
              case valueNodePattern(_*) => {
                getStatementFromValueNode(true)
              }
            }
          }
          case a =>
        }
      }
      statementAndRanks.++=(statements.map((_, rank)))
    }
    return statementAndRanks.toList
  }

  val valueNodeGeoPattern = """<http://www.wikidata.org/entity/VC.*>""".r
  val valueNodeNumericalPattern = """<http://www.wikidata.org/entity/VQ.*>""".r
  val valueNodeTimePattern = """<http://www.wikidata.org/entity/VT.*>""".r
  val valueNodeNumericalQuantityProperty = "<http://www.wikidata.org/ontology#numericValue>"
  val valueNodeTimeQuantityProperty = "<http://www.wikidata.org/ontology#time>"
  val valueNodeGeoLatitudeProperty = "<http://www.wikidata.org/ontology#latitude>"
  val valueNodeGeoLongitudeProperty = "<http://www.wikidata.org/ontology#longitude>"

  def getLiteralMap: Map[String, String] = {
    val literalMap = mutable.Map[String, String]()
    for ((value, tupleList) <- valueNodeToValuesMap) {
      value match {
        case valueNodeTimePattern(_*) => {
          tupleList.find((propAndObj) => propAndObj._1 == valueNodeTimeQuantityProperty) match {
            case Some((property, objectValue)) => literalMap += (value -> objectValue)
            case None => println("Value node of type Time, didn't have a time value")
          }
        }
        case valueNodeNumericalPattern(_*) => {
          tupleList.find { case (prop, obj) => prop == valueNodeNumericalQuantityProperty } match {
            case Some((prop, objectValue)) => literalMap += (value -> objectValue)
          }
        }
        case valueNodeGeoPattern(_*) => {
          tupleList.find { case (prop, obj) => prop == valueNodeGeoLatitudeProperty } match {
            case Some((prop, objectValue)) => literalMap += (value + "la" -> objectValue)
          }
          tupleList.find { case (prop, obj) => prop == valueNodeGeoLongitudeProperty } match {
            case Some((prop, objectValue)) => literalMap += (value + "lo" -> objectValue)
          }
        }
        case a => println(s"Unknown value node: $a")
      }
    }
    return literalMap.toMap
  }


  def main(args: Array[String]) {
    println("Following is the content read:")
//    var lines = Source.fromFile("/home/espen/prog/java/Wikidata-Toolkit-master/results/wikidata-statements.nt.gz", enc = "utf-8")
    def gis(s: String): InputStream = return new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))
    var lines = Source.fromInputStream(gis("/home/espen/prog/java/Wikidata-Toolkit-master/results/wikidata-statements.nt.gz"), enc = "utf-8")//.fromFile("/home/espen/prog/java/Wikidata-Toolkit-master/results/wikidata-statements.nt.gz", enc = "utf-8").getLines()
    var count = 10000
    var s = new StringBuilder()
    var iter = lines.iter
    while(count > 0) {
      iter.next() match {
        case '\n' => println(decodeLine(s.toString()));s.clear()
        case c => s.append(c)
      }
      count -= 1
    }
    print(s.toString())
    val printWriter = new PrintWriter("input/smallFiles/wikidata-statements" + fileNumber + ".nt")
    printWriter.write(s.toString())
    printWriter.close()
    //    for (line <- lines) {
//      appendBuffer(line)
//    }
  }

  def appendBuffer(line: String): Unit = {
    counter += 1
    if (counter % StatementsPerFile == 0) {
      val printWriter = new PrintWriter("input/smallFiles/wikidata-statements" + fileNumber + ".nt")
      fileNumber += 1
      printWriter.write(stringBuffer.toString())
      printWriter.close()
      stringBuffer.clear()
      println(s"@ file number: $fileNumber")
    }
    val cleanedLine = cleanUpLine(line)
    if(cleanedLine != NON_FIXABLE_URI) stringBuffer.append(cleanedLine + "\n")
  }
  def cleanUpLine(line: String) : String = {
    val matches: MatchIterator = totalPattern.findAllIn(line)
    if (!matches.hasNext) {
      return removeCAtTheEndOfProperties(line)
    }
    else {
      val modifiedLine = fixURI(line, matches)
      print("Modified line: " + modifiedLine)
      if (modifiedLine != NON_FIXABLE_URI) {
        return removeCAtTheEndOfProperties(modifiedLine)
      }
      else return modifiedLine
    }
  }

  //Remove c from the end of properties, eg: <http://www.wikidata.org/entity/Q4537983> <http://www.wikidata.org/entity/P1687c> <http://www.wikidata.org/entity/P1553>
  def removeCAtTheEndOfProperties(line: String): String = {
    val matches: MatchIterator = propertyEndWithCPattern.findAllIn(line)
    val listMatches = matches.toList
    if (listMatches.length == 0) return line
    val modifiedProperty = fixProperty(listMatches(0))
    return line.replace(listMatches(0), modifiedProperty)
  }

  def fixProperty(prop: String): String = {
    var cToRemove = 1
    for (c <- prop.dropRight(1).reverse) {
      if (c.isDigit) return prop.dropRight(cToRemove) + ">"
      cToRemove += 1
    }
    throw new Exception("Unable to fix prop: " + prop)
  }

  def fixURI(line: String, matches: MatchIterator): String = {
    var newURI = ""
    for (oldURI <- matches) {
      //println("error URI:" + oldURI)
      val subMatches = subPattern.findAllIn(oldURI)
      var counter = 0
      for (subValue <- subMatches) {
        subValue match {
          case "{" => println("removing {"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case "}" => println("removing }"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case "`" => println("removing `"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "%60") else oldURI.replaceAllLiterally(subValue, "%60")
          case "|" => println("removing `"); return NON_FIXABLE_URI
          case "^" => println("removing `"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "%5E") else oldURI.replace(subValue, "%5E")
          case "\\\"" => println("removing \\\""); return NON_FIXABLE_URI
          case "\\\\" => println("removing \\\\"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case "\\n" => println("removing \\n"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case " " => println("removing (space)"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "%20") else oldURI.replaceAllLiterally(subValue, "%20")
          case a => println("not removing...." + a)
        }
        counter += 1
      }
//      if (bracketsPattern.findAllIn(line).hasNext) {
//        return NON_FIXABLE_URI
//      }
      return line.replace(oldURI, newURI)
    }
    throw new Exception("Found an unknown pattern...")
  }
}

