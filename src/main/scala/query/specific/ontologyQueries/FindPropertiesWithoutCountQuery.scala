package query.specific.ontologyQueries

import query.{FindSubject, SimilarPropertyOntologyQuery}

/**
  * Created by Espen on 04.11.2016.
  */
import query.specific.ontologyQueries.FindPropertiesWithoutCountQuery.getQuery
class FindPropertiesWithoutCountQuery() extends SimilarPropertyOntologyQuery(getQuery) with FindSubject{


}

object FindPropertiesWithoutCountQuery {
  def getQuery() : String = {
    throw new Exception("Do not think this method is that necessary... see sparqlQueriesLocalWikidata for actual sparql syntax if needed")
    return "SELECT ?v \nWHERE {\n  ?v ?predicate ?object\n  filter(?object = @en)\n}"//TODO:Fix

  }
}
