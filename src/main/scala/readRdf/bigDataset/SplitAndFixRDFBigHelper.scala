package readRdf.bigDataset

import java.io.PrintWriter

import globals.MyDatasets
import query.Query
import query.specific.UpdateQueryFactory
import query.variables.DynamicQueryVariable

/**
  * Created by espen on 16.02.17.
  */
object SplitAndFixRDFBigHelper {
  abstract class RDFLine
  case class NewEntityLine(entity : String, property : String, statementID: String) extends RDFLine
  case class EntityLine(entity : String, property : String, statementID: String) extends RDFLine
  case class StatementLine(statementId : String, property : String, value: String) extends RDFLine
  case class ValueNodeLine(valueId: String, property : String, value: String) extends RDFLine
  case class UnknownLine() extends RDFLine

  val entityPattern = """<http://www.wikidata.org/entity/Q\d+>""".r
  val statementIdPattern = """<http://www.wikidata.org/entity/Q\d+.*>""".r
  val valueNodePattern = """<http://www.wikidata.org/entity/V.*>""".r

  def splitLine(line: String) : (String, String, String) = {
    if(line.split(" ").length == 4) {
      val Array(subj,predicate,objectValue,_) = line.split(" ")
      return (subj, predicate, objectValue)
    }
    val subj::predicate::theRest = line.split(" ").toList
    val objectValue = theRest.dropRight(1).mkString(" ")
    return (subj, predicate, objectValue)
    //Object is most likely a literal value

  }

  def findLineType(line : String) : RDFLine = {
    val (subj, predicate, objectValue) = splitLine(line)
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

  def uploadToDataset(s : Iterable[String]) = {
    UpdateQueryFactory.addStatements(s, dataset = MyDatasets.DsBig)
  }

  def createStatementNt(s: String, p: String, o: String) : String= {
    List(s, p, o).mkString(" ") + " .\n"
  }

  def getLiteralValueFromDB(value: String) : Option[String]= {
    val queryString = s"select ?o\n where { $value $valueNodeConnectorProperty ?o }"
    val query = new Query(() => queryString, MyDatasets.valueNodeDs)
    query.execute()
    try {
      val result = query.getResults(new DynamicQueryVariable("o", false))
      return Some(result(0))
    } catch {
      case a : IndexOutOfBoundsException => return None
    }
  }
  def uploadValueNodeStatements(s : Iterable[String]) = {
    UpdateQueryFactory.addStatements(s, MyDatasets.valueNodeDs)
  }

  val valueNodeConnectorProperty = "<http://www.espenalbert.com/rdf/wikidata/valueNodeConnector>"

}
