package core.query.specific

import core.globals.{KnowledgeGraph, SimilarPropertyOntology}
import core.globals.KnowledgeGraph.KnowledgeGraph

/**
  * Created by espen on 24.03.17.
  */
object QueryHelper {
  def getSameTypeFilterForQueryVariable(qVariable: String, rdfTypes: List[String])(implicit knowledgeGraph: KnowledgeGraph) : String = {
      s"""
         |  ?$qVariable <${KnowledgeGraph.getTypeProperty(knowledgeGraph)}> ?v .
         |  ${getSameTypeFilter("v", rdfTypes)}
        """.stripMargin
  }
  def getSameTypeFilter(qVariable: String, rdfTypes: List[String]) : String = {
    val filteredTypes= rdfTypes.filterNot(_ == SimilarPropertyOntology.standardRdfTypeDBpedia.toString)
    s"""
       |  filter(?$qVariable IN(<${filteredTypes.mkString(">,<")}>)) .
     """.stripMargin
  }

}
