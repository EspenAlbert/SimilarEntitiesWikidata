package core.rdf

import core.globals.KnowledgeGraphs
import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.ValueMatchCountFinder
import core.query.specific.QueryFactoryJena

/**
  * Created by espen on 04.05.17.
  */
object TypeCounter {

  def main(args: Array[String]): Unit = {
    implicit val kg = KnowledgeGraphs.wikidata
    storeTotalCountForTypeValues
  }
  def storeTotalCountForTypeValues(implicit knowledgeGraph : KnowledgeGraph): Unit = {
    val allTypes = QueryFactoryJena.findAllPossibleTypes()
    allTypes.foreach(findGlobalCountOfEntitiesOfType)


  }

  def findGlobalCountOfEntitiesOfType(typeValue : String)(implicit knowledgeGraph : KnowledgeGraph): Option[Int] = {
    val typeProperty = KnowledgeGraphs.getTypeProperty(knowledgeGraph)
    return ValueMatchCountFinder.valueIsAPotentialValueMatchFindCount(typeValue, typeProperty, false)

  }
}
