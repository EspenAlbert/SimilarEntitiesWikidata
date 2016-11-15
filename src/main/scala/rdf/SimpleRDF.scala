package rdf

/**
  * Created by Espen on 04.11.2016.
  */
import query.variables.{DynamicQueryVariable, QueryVariable, StaticQueryVariable}

import scala.collection.mutable.ArrayBuffer
class SimpleRDF(val s: QueryVariable = new DynamicQueryVariable("s", false), val p: QueryVariable = new DynamicQueryVariable("p", false), val o: QueryVariable = new DynamicQueryVariable("o", false), val filterLine: Boolean = false) {

//
//  def this(tuple3: Tuple3[String, String, String]) {
//    this(new StaticQueryVariable(tuple3._1), new StaticQueryVariable(tuple3._2), new StaticQueryVariable(tuple3._3))
//  }
  private def listOfElements: List[QueryVariable] = List[QueryVariable](s, p, o)

  private val distinctPattern = "distinct ?[^\\s]*".r

  def selectPhrase(): String = {
    val select =  s.getSelectPhrase + " " + p.getSelectPhrase + " " + o.getSelectPhrase
    return distinctPattern.findFirstIn(select) match {
      case Some(s) => s
      case None => select
    }
  }

  def wherePhrase(): String = {
    val whereStatement = ArrayBuffer[String]()
    for (queryVariable <- listOfElements) {
      whereStatement.append(queryVariable.getWherePhrase)
    }
    if(filterLine) {
      return whereStatement.mkString(" ") + " ."
    }
    val filterLines = ArrayBuffer[String]()
    for (queryVariable <- listOfElements) {
      queryVariable match {
        case c: DynamicQueryVariable => filterLines ++= c.getFilterLines
        case _ => Unit
      }
    }
    return whereStatement.mkString(" ") + " .\n" + filterLines.mkString("\n")
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