package query.specific

import query.{FindProperty, MultipleGraphQuery}

/**
  * Created by Espen on 04.11.2016.
  */
import query.specific.FindAllDistinctPropertiesQuery.findProperties
class FindAllDistinctPropertiesQuery extends MultipleGraphQuery(findProperties) with FindProperty{
}

object FindAllDistinctPropertiesQuery {
  val findProperties = "prefix w: <http://www.wikidata.org/entity/>\n" +
    "select distinct ?p \n" +
    "{ ?s ?p ?o . }"
}

