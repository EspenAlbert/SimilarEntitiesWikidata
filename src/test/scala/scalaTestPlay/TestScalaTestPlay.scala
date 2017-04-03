package scalaTestPlay

import org.scalatest.FunSuite
import tags.ActiveTag

import scala.collection.mutable.ListBuffer

/**
  * Created by espen on 16.03.17.
  */
class TestScalaTestPlay extends FunSuite{
  def fixture = new {
    val builder = new StringBuilder("Scala test is")
    val buffer = new ListBuffer[String]
  }
  test("testing fixture", ActiveTag) {
    val f = fixture
    f.builder.append(" easy")
    assertResult("Scala test is easy"){f.builder.toString()}
    assert(f.buffer.isEmpty)
    f.buffer.append("test")
    assert(f.buffer.nonEmpty)
  }
  test("fixture reset after previous test", ActiveTag) {
    val f = fixture
    assert(f.buffer.isEmpty)
  }
  test("iterating over null should work"){
    var eIsObject : (List[String], List[String]) = (Nil, Nil)
    for(a <- eIsObject._1) {
      println(a)
    }
  }

}
