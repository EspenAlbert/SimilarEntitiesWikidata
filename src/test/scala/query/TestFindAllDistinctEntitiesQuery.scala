package query

import org.scalatest.FunSuite
import query.specific.FindAllDistinctEntitiesQuery

/**
  * Created by Espen on 02.11.2016.
  */
class TestFindAllDistinctEntitiesQuery extends FunSuite{
  test("In total there should be: # of entities") {
    val queryForAllEntities = new FindAllDistinctEntitiesQuery()
    queryForAllEntities.execute()
    assert(queryForAllEntities.getSubjects().length == 5000000)
  }
}
