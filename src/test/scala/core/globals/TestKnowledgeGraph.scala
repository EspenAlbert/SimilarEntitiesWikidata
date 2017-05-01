package core.globals

import data.WikidataFactory
import org.scalatest.FunSuite

/**
  * Created by espen on 28.04.17.
  */
class TestKnowledgeGraph extends FunSuite{

  test("getPropertyQualifier") {
    val raw = WikidataFactory.qualifierProperties.head
    val expected = "http://www.wikidata.org/entity/P1734"
    val actual = KnowledgeGraph.getPropertyWithoutQualifierName(raw)
    assert(expected == actual)

  }

}
