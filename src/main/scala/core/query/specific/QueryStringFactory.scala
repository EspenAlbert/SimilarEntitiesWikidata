package core.query.specific

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.QueryFactory.executeQuery

/**
  * Created by espen on 02.05.17.
  */
object QueryStringFactory {

  def distinctPropertiesWhereObject(objectValue : String) : String =
    s"""
       |SELECT distinct ?p
       |WHERE {
       |  ?s ?p <$objectValue>
       |}
     """.stripMargin



}
