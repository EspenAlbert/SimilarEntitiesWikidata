package query.specific

/**
  * Created by Espen on 02.11.2016.
  */
import query.specific.FindSubjectStatementsQuery.getQuery
import query.{FindProperty, FindSubject, MultipleGraphQuery}
class FindSubjectStatementsQuery(val entity: String) extends MultipleGraphQuery(getQuery(entity)) with FindSubject with FindProperty{


}

object FindSubjectStatementsQuery {
  val findSubjects = "prefix w: <http://www.wikidata.org/entity/>\n" +
    "select *\n" +
    "{ ?s ?p w:Q76 . }"
  def getQuery(entity: String): String = {
    return findSubjects.replace("Q76", entity)
  }
}
