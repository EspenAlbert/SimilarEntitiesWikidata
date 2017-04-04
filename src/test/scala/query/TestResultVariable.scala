package query

import core.globals.SimilarPropertyOntology
import core.query.variables.ResultVariable
import org.scalatest.FunSuite
import tags.ActiveTag

/**
  * Created by espen on 04.04.17.
  */
class TestResultVariable extends FunSuite{
  test("Getting a boolean from a result variable", ActiveTag) {
    val rawStrings = List("true", "false", s""""true"^^<${SimilarPropertyOntology.datatypeBoolean}>""")
    val expectedResults = List(true,false,true)
    val actualResults : List[Boolean]= rawStrings.map(s => ResultVariable.getBoolean(new ResultVariable(s)))
    assert(expectedResults == actualResults)
  }
  test("Getting a double from a result variable", ActiveTag) {
    val rawStrings = List("0.021", s""""21.2"^^<${SimilarPropertyOntology.datatypeDouble}>""")
    val expected = List(0.021, 21.2)
    val actual = rawStrings.map(s => ResultVariable.getDouble(new ResultVariable(s)))
    assert(expected == actual)

  }

}
