package query.specific.ontologyQueries

import query.{FindSingleValue, NamedGraphQuery}

/**
  * Created by Espen on 04.11.2016.
  */
import FindIDForPropertyLabelQuery.{getQuery, graphName}
class FindIDForPropertyLabelQuery(propertyClass: String) extends NamedGraphQuery(getQuery(propertyClass), graphName) with FindSingleValue{


}

object FindIDForPropertyLabelQuery {
  def getQuery(propertyLabel : String) : String = {
    return "SELECT ?v \nWHERE {\n  ?v ?predicate ?object\n  filter(?object = \"" + propertyLabel + "\"@en)\n}"

  }
  val graphName: String = "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology"
}
