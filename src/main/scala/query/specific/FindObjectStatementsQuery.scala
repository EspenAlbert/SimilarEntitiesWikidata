package query.specific

import query.specific.FindObjectStatementsQuery.getQuery
import query.{FindObject, FindProperty, MultipleGraphQuery}
class FindObjectStatementsQuery(val entity: String) extends MultipleGraphQuery(getQuery(entity)) with FindObject with FindProperty{


}

object FindObjectStatementsQuery {
  val findObjects = "prefix w: <http://www.wikidata.org/entity/>\n" +
    "select *\n" +
    "{ w:Q76 ?p ?o . }"
  def getQuery(entity: String): String = {
    return findObjects.replace("Q76", entity)
  }
}
