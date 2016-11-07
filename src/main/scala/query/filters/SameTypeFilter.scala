package query.filters

import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.SimpleRDF

/**
  * Created by Espen on 07.11.2016.
  */
class SameTypeFilter(val rdfType : String, variable : DynamicQueryVariable) extends QueryFilter{
  override def getSparql: String = {
    return new SimpleRDF(variable, new StaticQueryVariable("w:P31"), new StaticQueryVariable(rdfType)).wherePhrase()
  }
}
