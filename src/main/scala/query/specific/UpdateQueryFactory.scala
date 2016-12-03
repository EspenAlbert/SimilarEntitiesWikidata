package query.specific

import globals.SimilarPropertyOntology
import jenaQuerier.QueryLocalServer
import rdf.SimpleRDF

/**
  * Created by Espen on 01.12.2016.
  */
object UpdateQueryFactory {
  def addToLocalDataset(statements: SimpleRDF*): String = {
    val statementsInsert = statements.map(_.getStatementNt()).mkString("\n")
    val insertLine = "insert { \n %s } \n where {}".format(statementsInsert)
    try {
      QueryLocalServer.updateLocalData(insertLine)
    } catch{
      case e => println("Failed to update: " + insertLine)
    }
    insertLine
  }

  def updateValueCount(propertyAsFullString: String, entity: String, count: Int) = {
    val updateQuery = s"insert { <$propertyAsFullString> <${SimilarPropertyOntology.valueMatchProperty}> [ <${SimilarPropertyOntology.valueMatchValue}> <$entity>;\n" +
      s"""<${SimilarPropertyOntology.valueMatchCount}> "%d" ] } where {}""".format(count)
    println(updateQuery)
    QueryLocalServer.updateLocalData(updateQuery)
  }
}
