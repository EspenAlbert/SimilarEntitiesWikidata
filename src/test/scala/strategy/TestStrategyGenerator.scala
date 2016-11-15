package strategy

import org.scalatest.FunSuite
import rdf.GraphRDF
import strategies.StrategyGenerator

/**
  * Created by Espen on 11.11.2016.
  */
class TestStrategyGenerator extends FunSuite{
  test("The correct strategies for obama are created") {
    val strategies = StrategyGenerator.generateStrategies(new GraphRDF("w:Q76"))
    print(strategies)
    assert(strategies.length == 2)
  }

}
