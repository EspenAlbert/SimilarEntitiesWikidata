package structure.pathsBetweenEntities

import org.scalatest.FunSuite
import structureFinder.pathsBetweenEntities.QueryPathBuilder

/**
  * Created by espen on 28.04.17.
  */
class TestQueryPathBuilder extends FunSuite {
  test("should be able to generate filters") {
    val entities = QueryPathBuilder.createEntityString()(1).split(" ")
    val filter = QueryPathBuilder.generateFilter(entities.tail.init)
    println(filter)
  }

}
