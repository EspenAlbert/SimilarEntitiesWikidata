package query

import rdf.SimpleRDF

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 07.11.2016.
  */
class WhereFilter(statements : SimpleRDF*) {
  private val countPattern = """\(count[^\)]*\) as [^\)]*\)""".r
  private var whereLines =  ArrayBuffer[String]()
  private val selectVariables = ArrayBuffer[String]()
  selectVariables.append("select")
  for(statement <- statements){
    selectVariables.append(statement.selectPhrase())
    whereLines.append(statement.wherePhrase())
  }
  def getSelect(): String = {
    val select: String = selectVariables.mkString(" ")
    countPattern.findFirstIn(select) match {//Could be changed to multiple counts if we want that option
      case Some(s) => return "select "  + s+ "\n"
      case None => return select+ "\n"
    }
  }
  def getWhereClause(): String = {
    return "{" + whereLines.mkString("\n") + "}"
  }


}
