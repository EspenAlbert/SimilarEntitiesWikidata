package query.specific

import query.{FindSubject, MultipleGraphQuery}

/**
  * Created by Espen on 04.11.2016.
  */
import query.specific.FindAllDistinctEntitiesQuery.findEntities

class FindAllDistinctEntitiesQuery extends MultipleGraphQuery(findEntities) with FindSubject{
}

object FindAllDistinctEntitiesQuery {
  val findEntities = "prefix w: <http://www.wikidata.org/entity/>\n" +
    "select distinct ?s \n" +
    "{ ?s ?p ?o ." +
    "?s w:P31 ?u}"
}



