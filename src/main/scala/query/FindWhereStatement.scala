package query

import rdf.SimpleRDF

/**
  * Created by Espen on 04.11.2016.
  */
trait FindWhereStatement {
  def whereStatement(whereClause: SimpleRDF, query: String = "Prefix w: <http://www.wikidata.org/entity/>\nselect *\n{ ?s ?p ?o . }") :String = {
    var filteredQuery = query
    if(whereClause.s != null) filteredQuery = filteredQuery.replace("?s", whereClause.sInSparql)
    if(whereClause.p != null) filteredQuery = filteredQuery.replace("?p", whereClause.pInSparql)
    if(whereClause.o != null) filteredQuery = filteredQuery.replace("?o", whereClause.oInSparql)
    return filteredQuery
  }
}
