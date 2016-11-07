package query

import org.scalatest.FunSuite
import query.specific.FindSubjectStatementsQuery

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindSubjectStatementsQuery extends FunSuite{
  test("Subject statements for obama should be 410") {
    val obamaId = "Q76"
    val query = new FindSubjectStatementsQuery(obamaId)
    query.execute()
    val subjects: List[String] = query.getSubjects()
    assert(subjects.length == 410)
    print(subjects)
  }
  test("Subjects statements should have properties always starting with P, and for obama have 410") {
    val obamaId = "Q76"
    val query = new FindSubjectStatementsQuery(obamaId)
    query.execute()
    val properties: List[String] = query.getProperties()
    assert(!properties.exists(_.startsWith("w:Q")))
    assert(properties.length == 410)
    print(properties)

  }

}
