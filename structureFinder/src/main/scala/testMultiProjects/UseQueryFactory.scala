package testMultiProjects

import core.globals.KnowledgeGraph
import core.query.specific.QueryFactory
import iAndO.factoryMethods.IOFactory

/**
  * Created by espen on 01.05.17.
  */
object UseQueryFactory {
  def main(args: Array[String]): Unit = {
    println("Hello from structure finder build..")
    implicit val kg = KnowledgeGraph.wikidata
    val entities = QueryFactory.find100SamplesForProperty(IOFactory.getAllItemProperties.head)
    entities.foreach(println)
  }

}
