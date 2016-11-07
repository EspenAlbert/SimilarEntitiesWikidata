package query

import org.scalatest.FunSuite
import query.specific.FindSubjectsOfType

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindSubjectsOfType extends FunSuite{
  test("Should be exactly 101 entities of country") {
    val query = new FindSubjectsOfType("w:Q6256")
    query.execute()
    val subjects: List[String] = query.getSubjects()
    assert(subjects.length == 101)
    print(subjects)
  }

}
