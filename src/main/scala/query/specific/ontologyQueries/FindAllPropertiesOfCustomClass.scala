package query.specific.ontologyQueries

import query.{FindSubject, SimilarPropertyOntologyQuery}

/**
  * Created by Espen on 04.11.2016.
  */
import query.specific.ontologyQueries.FindAllPropertiesOfCustomClass.getQuery

class FindAllPropertiesOfCustomClass() extends SimilarPropertyOntologyQuery(getQuery) with FindSubject{


}

object FindAllPropertiesOfCustomClass {
  def getQuery() : String = {
    return "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\nprefix w: <http://www.wikidata.org/entity/>\n\nSELECT distinct ?s\nWHERE {\n  { \n     ?s rdf:type <http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#OWLClass_74eb1d1f_d088_46df_8a75_43ed50501f1d>\n  }}"
  }
}

