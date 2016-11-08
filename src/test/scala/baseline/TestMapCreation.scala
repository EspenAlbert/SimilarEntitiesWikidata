package baseline

import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestMapCreation extends FunSuite{
  test("A file should be created properly") {
    val map = PropertyIdfCalculator.getMap()
    assert(map.keys.toList.length == 2398)
  }


}
