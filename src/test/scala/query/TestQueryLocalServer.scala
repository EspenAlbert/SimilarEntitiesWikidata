package query

import java.io.ByteArrayOutputStream

import org.scalatest.FunSuite
import query.jenaQuerier.QueryLocalServer

/**
  * Created by Espen on 02.11.2016.
  */
class TestQueryLocalServer extends FunSuite{
  test("Manage to query all graphs in the database") {
    val query: String = "prefix w: <http://www.wikidata.org/entity/>\n" + "select *\n" + "{ {\n" + "?s ?p w:Q76 . } UNION {\n" + "    w:Q76 ?p ?o . }\n" + "}"
    val whereClause: String = "\n{ {\n" + "?s ?p w:Q76 . } UNION {\n" + "    w:Q76 ?p ?o . }\n" + "}"
    val whereClauseActual: String = QueryLocalServer.getWhereClause(query)
    assert( whereClause.contains(whereClauseActual))
    print(whereClauseActual)
    String.format("where query is not properly extracted: %s ", whereClauseActual)
  }

  test("Create multiple graph query should work") {
    val oneGraphQuery: String = "prefix w: <http://www.wikidata.org/entity/>\n" + "select *\n" + "{ {\n" + "?s ?p w:Q76 . } UNION {\n" + "    w:Q76 ?p ?o . }\n" + "}"
    val multiGraphQuery: String = "prefix w: <http://www.wikidata.org/entity/>\n" + "select *\n" + "{ {\n" + "?s ?p w:Q76 . } UNION {\n" + "    w:Q76 ?p ?o . } \n" + "UNION \n" + " { GRAPH ?g " + "{{ ?s ?p w:Q76 . } \n" + "    UNION{\n" + "w:Q76 ?p ?o . } \n" + "    } }\n" + "}"
    val fosExpected: ByteArrayOutputStream = new ByteArrayOutputStream
    val fosActual: ByteArrayOutputStream = new ByteArrayOutputStream
    val multiGraphQueryCreated: String = QueryLocalServer.convertToMultipleGraphQuery(oneGraphQuery)
    QueryLocalServer.query(multiGraphQuery, fosExpected)
    QueryLocalServer.query(multiGraphQueryCreated, fosActual)
    System.out.println(fosActual.toString)
    assert(fosExpected.toString == fosActual.toString)
  }

}
