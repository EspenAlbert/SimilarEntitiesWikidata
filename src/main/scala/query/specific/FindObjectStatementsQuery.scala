package query.specific

import query.{FindObject, FindProperty, Query}
import query.specific.FindObjectStatementsQuery.getQuery
class FindObjectStatementsQuery(val entity: String) extends Query(getQuery(entity)) with FindObject with FindProperty{


}

object FindObjectStatementsQuery {
  val findSubjects = "prefix w: <http://www.wikidata.org/entity/>\n" +
    "select *\n" +
    "{ w:Q76 ?p ?o . }"
  def getQuery(entity: String): String = {
    return findSubjects.replace("Q76", entity)
  }
}
