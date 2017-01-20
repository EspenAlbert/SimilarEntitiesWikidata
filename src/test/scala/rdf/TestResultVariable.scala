package rdf

import org.scalatest.{BeforeAndAfter, FunSuite}
import query.variables.ResultVariable

/**
  * Created by Espen on 02.11.2016.
  */
class TestResultVariable extends FunSuite with BeforeAndAfter{

  test("a result variable should be converted to int without problem") {
    import ResultVariable.getInt
    val variable = new ResultVariable("\"500\"")
    assert((variable + 5) == 505) //INt
    assert(variable.length() == 3)//String
    assert(variable.indexOf("0") == 1)
  }


}
