package strategy

import core.globals.KnowledgeGraph
import core.rdf.GraphRDF
import org.scalatest.FunSuite
import core.strategies.{PropertyMatchStrategy, Strategy}

/**
  * Created by Espen on 11.11.2016.
  */
class TestPropertyMatchStrategy extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("Sorting a strategy should work") {
    val strategy = PropertyMatchStrategy("http://www.wikidata.org/entity/P991",false,List("http://www.wikidata.org/entity/Q5"), 199)
    val strategy2 = PropertyMatchStrategy("http://www.wikidata.org/entity/P244",true,List("http://www.wikidata.org/entity/Q5"), 290)
    val list = List[Strategy](strategy2, strategy)
    val sortedList = list.sorted
    assert(sortedList(0) == strategy)
    assert(sortedList(1) == strategy2)
  }
  test("P1591 strategy find similars defendant") {
    val strategy = PropertyMatchStrategy("http://www.wikidata.org/entity/P1591",false,List("http://www.wikidata.org/entity/Q5"), 555)
    val similars = strategy.findSimilars()
    print(similars)
    assert(similars.keys.size== 4)
  }

}
