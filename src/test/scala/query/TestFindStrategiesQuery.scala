package query

import core.strategies.StrategyFactory
import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindStrategiesQuery extends FunSuite{
  test("w:P31 should have 4 core.strategies returned") {
    val property = "w:P31"
    val properties: List[String] = StrategyFactory.getStrategies(property)
    assert(properties.length == 4)
    print(properties)
  }


}
