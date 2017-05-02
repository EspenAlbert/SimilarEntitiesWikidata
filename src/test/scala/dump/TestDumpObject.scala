package dump

import core.dump.DumpObject
import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestDumpObject extends FunSuite{
  test("dumping a map should work as expected") {
    val map = Map[String, Double]( ("a" -> 0.512d), "b" -> 0.515d)
    DumpObject.dumpJsonMap(map, "test1")
    val a = DumpObject.loadJsonMap("test1")
    assert(a == map)

  }
}
