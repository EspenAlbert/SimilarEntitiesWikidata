package query

import rdf.SimpleRDF

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 07.11.2016.
  */
import WhereFilter.filterAwayMultipleOfSameSelect
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
    val filteredSelect = filterAwayMultipleOfSameSelect(select)
    countPattern.findFirstIn(select) match {//Could be changed to multiple counts if we want that option
      case Some(s) => return "select "  + filteredSelect+ "\n"
      case None => return filteredSelect+ "\n"
    }
  }
  def getWhereClause(): String = {
    return "{" + whereLines.mkString("\n") + "}"
  }


}

object WhereFilter {
  private val selectPattern = """(distinct )?\?[^\s]""".r
  private val countStatement = """\(count\((distinct )?\?[^\s]*\) as \?[^\s]*\)""".r

  def filterAwayMultipleOfSameSelect(select: String): String = {
    val adjustedSelect = ArrayBuffer[String]()
    adjustedSelect.append("select")
    for (m <- selectPattern.findAllMatchIn(select)) {
      val substring: String = m.group(0)
      if (!adjustedSelect.contains(substring)) {
        adjustedSelect.append(substring)
      }
    }
    countStatement.findFirstIn(select) match {
      case Some(s) => return s
      case None => return adjustedSelect.mkString(" ")
    }
  }
}
