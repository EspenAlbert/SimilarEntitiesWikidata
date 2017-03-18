package query

import core.globals.KnowledgeGraph
import core.query.specific.QueryFactory
import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindObjectStatementsQuery extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("Object statements for obama should be 135") {
    val obamaId = "Q76"
    val objects: List[String] = QueryFactory.findSubjectsAndProperties(obamaId)._2
    assert(objects.length == 135)
    print(objects)
  }
  test("Subject statements should have properties always starting with P, and for obama have 135") {
    val obamaId = "Q76"
    val properties: List[String] = QueryFactory.findSubjectsAndProperties(obamaId)._1
    assert(!properties.exists(_.startsWith("w:Q")))
    assert(properties.length == 135)
    print(properties)

  }

}
