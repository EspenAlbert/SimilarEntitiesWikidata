package core.rdf

import core.globals.KnowledgeGraph.KnowledgeGraph

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by espen on 18.04.17.
  */
class GraphRDFDescriptivePropertyChecker(entity: String)(implicit knowledgeGraph: KnowledgeGraph) extends GraphRDF(entity){
  override val entityIsObjectStatements = Future{
    throw new NotImplementedError("todo")
  }

}
