package query.specific.ontologyQueries

import query.{FindSingleValue, FindWhereStatement, MultipleGraphQuery}
import rdf.SimpleRDF

/**
  * Created by Espen on 04.11.2016.
  */
import FindCountWhereQuery.getQuery
class FindCountWhereQuery(whereStatement: SimpleRDF) extends MultipleGraphQuery(getQuery(whereStatement)) with FindSingleValue{

}

object FindCountWhereQuery extends FindWhereStatement{
  def getQuery(statement: SimpleRDF): String = {
    val query = "prefix w: <http://www.wikidata.org/entity/>\nselect (count(*) as ?v)\n{ ?s ?p ?o . }"
    return whereStatement(statement, query)
  }
}


