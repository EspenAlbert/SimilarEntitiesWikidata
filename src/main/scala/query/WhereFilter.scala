package query

import rdf.SimpleRDF

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 07.11.2016.
  */
class WhereFilter(statements : SimpleRDF*) {
  private var whereLines =  ArrayBuffer[String]()
  private val selectVariables = ArrayBuffer[String]()
  selectVariables.append("select")
  for(statement <- statements){
    selectVariables.append(statement.selectPhrase())
    whereLines.append(statement.wherePhrase())
  }
  def getSelect(): String = {
    return selectVariables.mkString(" ") + "\n"
  }
  def getWhereClause(): String = {
    return "{" + whereLines.mkString("\n") + "}"
  }


}
