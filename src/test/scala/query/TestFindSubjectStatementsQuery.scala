package query

import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindSubjectStatementsQuery extends FunSuite{
  test("Subject statements for obama should be 410") {
    val obamaId = "Q76"
    val query = new FindSubjectStatementsQuery(obamaId)
    query.execute()
    assert(query.getSubjects().length == 410)

  }

}
