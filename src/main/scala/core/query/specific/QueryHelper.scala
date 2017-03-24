package core.query.specific

import core.globals.KnowledgeGraph
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
    s"""
       |  filter(?$qVariable IN(<${rdfTypes.mkString(">,<")}>)) .
     """.stripMargin
  }

}
