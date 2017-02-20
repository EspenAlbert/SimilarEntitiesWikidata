package readRdf

import java.io.{BufferedInputStream, FileInputStream, InputStream, PrintWriter}
import java.util.zip.GZIPInputStream

import globals.MyDatasets
import org.apache.jena.query.QueryParseException
import query.Query
import query.specific.UpdateQueryFactory
import query.variables.DynamicQueryVariable
import readRdf.SplitAndFixRDFBig.CustomOriginalPropertyTypes.CustomOriginalPropertyTypes

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex.MatchIterator
import readRdf.bigDataset.SplitAndFixRDFBigHelper._
import readRdf.bigDataset.URIFixer
/**
  * Created by Espen on 13.10.2016.
  */
object SplitAndFixRDFBig {
  var testActive = false
  var addEverything = true
  var fileNumber = 1

  def main(args: Array[String]) {
    //    var lines = Source.fromFile("/home/espen/prog/java/Wikidata-Toolkit-master/results/wikidata-statements.nt.gz", enc = "utf-8")
    def gis(s: String): InputStream = return new GZIPInputStream(new BufferedInputStream(new FileInputStream(s)))

    var lines = Source.fromInputStream(gis("/home/espen/prog/java/Wikidata-Toolkit-master/results/wikidata-statements.nt.gz"), enc = "utf-8")
    //.fromFile("/home/espen/prog/java/Wikidata-Toolkit-master/results/wikidata-statements.nt.gz", enc = "utf-8").getLines()
    var s = new StringBuilder()
    var iter = lines.iter
    val uploadEvery = 2000
    //    val statementBuffer= ListBuffer[String]()
    var i = 0
    var j = -1
    var printWriter = new PrintWriter(s"input/errorLog$fileNumber.txt")
    var errorNumber = 1
    while (iter.hasNext) {
      iter.next() match {
        case '\n' => {
          //          statementBuffer.append(s.toString())
          //          if(i%uploadEvery == 0) {
          i += 1
          s.clear()
          if(j > -1) {
            try {
              decodeLine(s.toString())
            } catch {
              case a: QueryParseException => {
                printWriter.write(s"Line $i : ${s.toString()} had ERROR NR $errorNumber : ${a.getMessage} \n\n\n\n statements: ${lastFinalStatements.mkString("\n")}")
                println(a.getMessage)
                errorNumber += 1
                if (errorNumber % 10 == 0) {
                  printWriter.close()
                  fileNumber += 1
                  printWriter = new PrintWriter(s"input/errorLog$fileNumber.txt")
                }
              }
            }
            s.clear()
            //            statementBuffer.clear()
            if(i%uploadEvery == 0) println(s"upload line nr: $i")
          }
          }

        //        }
        case c => s.append(c)
      }
    }
    println(s"In total $i number of lines")
    //      iter.next() match {
    //        case '\n' => decodeLine(s.toString());s.clear()
    //        case c => s.append(c)
    //      }
    //      count -= 1
    //    }
    //    print(s.toString())
    //    val printWriter = new PrintWriter("input/smallFiles/wikidata-statements" + fileNumber + ".nt")
    //    printWriter.write(s.toString())
    //    printWriter.close()
    //    for (line <- lines) {
    //      appendBuffer(line)
    //    }
  }

  val statementIdMap = mutable.Map[String, (String, String)]()
  val statementIdToValuesMap = mutable.Map[String, mutable.ListBuffer[(String, String)]]()
  val valueNodeToValuesMap = mutable.Map[String, mutable.ListBuffer[(String, String)]]()

  def decodeLine(line: String) = {
    findLineType(line) match {
      case l: NewEntityLine => addStatements()
      case l: EntityLine => statementIdMap += (l.statementID -> (l.entity, l.property))
      case l: StatementLine => statementIdToValuesMap.get(l.statementId) match {
        case Some(tuples) => tuples += Tuple2(l.property, l.value)
        case None => statementIdToValuesMap += (l.statementId -> mutable.ListBuffer((l.property, l.value)))
      }
      case l: ValueNodeLine => valueNodeToValuesMap.get(l.valueId) match {
        case Some(tuples) => tuples += Tuple2(l.property, l.value)
        case None => valueNodeToValuesMap += (l.valueId -> mutable.ListBuffer((l.property, l.value)))
      }
      case l: UnknownLine =>
    }
  }


  val preferredRank = "<http://www.wikidata.org/ontology#PreferredRank>"
  var lastFinalStatements : List[String] = null

  def addStatements() = {
    val literalMap = getLiteralMap
    if (SAVE_VALUE_NODES) uploadValueNodeStatements(createValueNodeStatements(literalMap))
    val statementsAndRanks = findStatementsAndRanks(literalMap)
    val propertiesWithPrefferedRank = statementsAndRanks.filter(_._2 == preferredRank).map(_._1._2).toSet
    val finalStatements = ListBuffer[String]()
    for (((s, p, o), rank) <- statementsAndRanks) {
      if (!propertiesWithPrefferedRank.contains(p)) finalStatements.append(createStatementNt(s, p, o))
      else {
        statementIdPattern.findFirstIn(o) match {
          case Some(objectValue) => finalStatements.append(createStatementNt(s, p, o))
          case None => if (rank == preferredRank) finalStatements.append(createStatementNt(s, p, o))
        }
      }
    }
    //    writeStatementsToFile(finalStatements)
    lastFinalStatements = finalStatements.toList
    if (!testActive) {
      try {
        uploadToDataset(finalStatements)
      } catch {
        case a : QueryParseException => uploadToDataset(URIFixer.fixFixableURIs(finalStatements))
      }
    }
    else {
      writeStatementsToFile(finalStatements)
    }
    statementIdMap.clear()
    statementIdToValuesMap.clear()
    valueNodeToValuesMap.clear()
  }

  object CustomOriginalPropertyTypes extends Enumeration {
    type CustomOriginalPropertyTypes = Value
    val statement = Value("statement")
    val qualifier = Value("qualifier")
    val latitude = Value("latitude")
    val longitude = Value("longitude")
  }

  val propertyValue = """<http://www.wikidata.org/entity/P\d+v>""".r
  val qualifierValue = """<http://www.wikidata.org/entity/P\d+q>""".r


  def findStatementsAndRanks(literalMap: Map[String, String]): List[((String, String, String), String)] = {
    val statementAndRanks = ListBuffer[((String, String, String), String)]()
    for ((statementID, tupleList) <- statementIdToValuesMap) {
      var rank: String = null
      var statements = ListBuffer[(String, String, String)]()
      val entity = statementIdMap.getOrElse(statementID, throw new Exception(s"StatementID had no original entity $statementID"))._1
      val originalProperty = statementIdMap.getOrElse(statementID, throw new Exception(s"StatementID had no original entity $statementID"))._2

      def createStatement(literalValue: String, changedOriginalProperty: CustomOriginalPropertyTypes = CustomOriginalPropertyTypes.statement,
                          qualifierProperty: String = null): Option[(String, String, String)] = {
        statementIdMap.get(statementID) match {
          case Some((subj, property)) => getCustomOriginalProperty(changedOriginalProperty, qualifierProperty) match {
            case Some(property) => Some(subj, property, literalValue)
            case None => None
          }
          case None => None
        }
      }

      def getCustomOriginalProperty(customType: CustomOriginalPropertyTypes, qualifierProperty: String = null): Option[String] = {
        customType match {
          case CustomOriginalPropertyTypes.statement => Some(originalProperty)
          case CustomOriginalPropertyTypes.latitude => Some(originalProperty.dropRight(1) + "la>")
          case CustomOriginalPropertyTypes.longitude => Some(originalProperty.dropRight(1) + "lo>")
          case CustomOriginalPropertyTypes.qualifier => Some(originalProperty.dropRight(1) + qualifierProperty + ">")
          case a => println("couldn't find property or you tried a custom orginial property value that don't exists!"); None
        }
      }

      for ((prop, value) <- tupleList) {
        def getStatementFromValueNode(qualifier: Boolean): (Option[(String, String, String)], Option[(String, String, String)]) = {
          val qualifierPropertyId = if (qualifier) prop.substring(prop.indexOf('P'), prop.length - 1) else null

          def createStatementNormal(literalValue: String): (Option[(String, String, String)], Option[(String, String, String)]) = {
            if (qualifier) ((createStatement(literalValue, CustomOriginalPropertyTypes.qualifier, qualifierPropertyId)), None)
            else ((createStatement(literalValue), None))
          }

          literalMap.get(value) match {
            case Some(literalValue) => createStatementNormal(literalValue)
            case None => (literalMap.get(value + "la"), literalMap.get(value + "lo")) match {
              //Means we have a coordinate statement
              case (Some(la), Some(lo)) =>
                if (!qualifier) {
                  (createStatement(la, CustomOriginalPropertyTypes.latitude), createStatement(lo, CustomOriginalPropertyTypes.longitude))
                }
                else {
                  (createStatement(la, CustomOriginalPropertyTypes.qualifier, qualifierPropertyId + "la"),
                    createStatement(lo, CustomOriginalPropertyTypes.qualifier, qualifierPropertyId + "lo"))
                }
              //last try the statement already exist somewhere else we need to use or db
              case (None, None) => {
                val literalValue = getLiteralValueFromDB(value) match {
                  case Some(lValue) => return createStatementNormal(lValue)
                  case None => println(s"No result for $entity $originalProperty $value"); return (None, None)
                }
                println("This should never happen");
                return (None, None)
              }
            }
          }
        }

        def createValueStatementAddToList(qualifier: Boolean) = {
          getStatementFromValueNode(qualifier) match {
            case (Some(a), Some(b)) => statements.append(a, b)
            case ((Some(a), None)) => statements.append(a)
            case (None, None) =>
          }
        }

        prop match {
          case "<http://www.wikidata.org/ontology#rank>" => rank = value
          case propertyValue(_*) => {
            value match {
              case valueNodePattern(_*) => {
                createValueStatementAddToList(false)
              }
              case literValue => {
                statements.append((entity, originalProperty, literValue))
              }
            }
          }
          case qualifierValue(_*) => {
            value match {
              case valueNodePattern(_*) => {
                createValueStatementAddToList(true)
              }
              case literValue => {
                statements.append((entity, originalProperty, literValue))
              }
            }
          }
          case a =>
        }
      }
      statementAndRanks.++=(statements.map((_, rank)))
      statements.clear()
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

  val SAVE_VALUE_NODES = true

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

  def createValueNodeStatements(literalMap: Map[String, String]): Seq[String] = {
    val valueNodeStatements = ListBuffer[String]()
    for ((valueNode, literalValue) <- literalMap) {
      var realValueNodeIRI: String = null
      if (valueNode.endsWith(">la")) {
        realValueNodeIRI = valueNode.replace(">la", "la>")
      }
      else if (valueNode.endsWith(">lo")) {
        realValueNodeIRI = valueNode.replace(">lo", "lo>")
      }
      else {
        realValueNodeIRI = valueNode
      }
      valueNodeStatements.append(s"$realValueNodeIRI $valueNodeConnectorProperty $literalValue .")
    }
    return valueNodeStatements
  }


  def writeStatementsToFile(s: Iterable[String]) = {
    //    val printWriter = new PrintWriter("input/smallFiles/" +(if(testActive) "/test" else "") + "wikidata-statements" + fileNumber + (if(testActive) "-test" else "") + ".nt")
    val printWriter = new PrintWriter(s"input/smallFiles/${if (testActive) "test/" else "run1/"}wikidata-statements$fileNumber.nt")
    for (l <- s) {
      printWriter.write(l)
    }
    printWriter.close()
    fileNumber += 1
  }
}



