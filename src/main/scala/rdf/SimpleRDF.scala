package rdf

/**
  * Created by Espen on 04.11.2016.
  */
import query.variables.{DynamicQueryVariable, QueryVariable, StaticQueryVariable}

import scala.collection.mutable.ArrayBuffer
class SimpleRDF(val s: QueryVariable = new DynamicQueryVariable("s", false), val p: QueryVariable = new DynamicQueryVariable("p", false), val o: QueryVariable = new DynamicQueryVariable("o", false)) {


  private def listOfElements: List[QueryVariable] = List[QueryVariable](s, p, o)

  def selectPhrase(): String = {
    return s.getSelectPhrase + " " + p.getSelectPhrase + " " + o.getSelectPhrase
  }

  def wherePhrase(): String = {
    val whereStatement = ArrayBuffer[String]()
    for (queryVariable <- listOfElements) {
      whereStatement.append(queryVariable.getWherePhrase)
    }
    val filterLines = ArrayBuffer[String]()
    for (queryVariable <- listOfElements) {
      queryVariable match {
        case c: DynamicQueryVariable => filterLines ++= c.getFilterLines
      }
    }
    return whereStatement.mkString(" ") + " ." + filterLines.mkString("\n")
  }

  def getStatementNt(): String = {
    val statement = ArrayBuffer[String]()
    for (variable <- listOfElements) {
      variable match {
        case v: StaticQueryVariable => statement.append(v.getNtValue())
        case v => throw new Exception("Cannot write nt from :" + v)
      }
    }
    return statement.mkString(" ") + "."
  }
}