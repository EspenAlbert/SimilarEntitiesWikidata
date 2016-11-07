package query

import org.scalatest.FunSuite
import query.specific.FindSubjectsWithProperty

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindSubjectsWithProperty extends FunSuite{
  test("Should be exactly 300 entities with gender property") {
    val query = new FindSubjectsWithProperty("w:P10")
    query.execute()
    val subjects: List[String] = query.getSubjects()
    assert(subjects.length == 300)
    print(subjects)
  }

}
