package query

import org.scalatest.FunSuite
import query.specific.{QueryFactory}

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindSubjectsWithProperty extends FunSuite{
  test("Should be exactly 300 entities with gender property") {
    val subjects: List[String] = QueryFactory.findSubjectsWithProperty("w:P21")
    assert(subjects.length == 300)
    print(subjects)
  }

}
