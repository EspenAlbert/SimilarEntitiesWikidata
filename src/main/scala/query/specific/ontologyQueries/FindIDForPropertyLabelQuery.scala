package query.specific.ontologyQueries

import query.{FindSingleValue, SimilarPropertyOntologyQuery}

/**
  * Created by Espen on 04.11.2016.
  */
import query.specific.ontologyQueries.FindIDForPropertyLabelQuery.getQuery
class FindIDForPropertyLabelQuery(propertyClass: String) extends SimilarPropertyOntologyQuery(getQuery(propertyClass)) with FindSingleValue{


}
object FindIDForPropertyLabelQuery {
  def getQuery(propertyLabel : String) : String = {
    return "SELECT ?v \nWHERE {\n  ?v ?predicate ?object\n  filter(?object = \"" + propertyLabel + "\"@en)\n}"

  }
}
