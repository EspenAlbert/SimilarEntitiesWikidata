package query.specific

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

}
