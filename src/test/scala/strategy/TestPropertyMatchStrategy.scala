package strategy

import org.scalatest.FunSuite
import rdf.GraphRDF
import strategies.{PropMatchStrategy, Strategy}

/**
  * Created by Espen on 11.11.2016.
  */
class TestPropertyMatchStrategy extends FunSuite{
  test("Sorting a strategy should work") {
    val strategy = PropMatchStrategy("http://www.wikidata.org/entity/P991",false,8.471149252914831,"http://www.wikidata.org/entity/Q5")
    val strategy2 = PropMatchStrategy("http://www.wikidata.org/entity/P244",true,4.007333185232471,"http://www.wikidata.org/entity/Q5")
    val list = List[Strategy](strategy2, strategy)
    val sortedList = list.sorted
    assert(sortedList(0) == strategy)
    assert(sortedList(1) == strategy2)
  }
  test("P1591 strategy find similars defendant") {
    val strategy = PropMatchStrategy("http://www.wikidata.org/entity/P1591",false, 8.55, "http://www.wikidata.org/entity/Q5")
    val similars = strategy.findSimilars()
    print(similars)
    assert(similars.length == 4)
    val features = strategy.execute(List(similars : _*).map((s) => new GraphRDF(s)))
    print(features)
    assert(features.keySet.toList.length == 4)
  }

}
