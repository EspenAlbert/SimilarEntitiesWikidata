package baseline

import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestMapCreation extends FunSuite{
  test("the map should be created properly from the server") {
    val map = PropertyIdfCalculator.getMap()
    assert(map.keys.toList.length == 2398)
  }
  test("it should be possible to dump the map to a file") {
    PropertyIdfCalculator.dumpToFile()
    val map = PropertyIdfCalculator.getMapFromFile()
    assert(map.keys.toList.length == 2397)
  }
  test("read directly from file") {
    val map = PropertyIdfCalculator.getMapFromFile()
    assert(map.keys.toList.length == 2398)
  }


}
