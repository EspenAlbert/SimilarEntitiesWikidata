package query.filters

import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.SimpleRDF

/**
  * Created by Espen on 07.11.2016.
  */
import SameTypeFilter.getStatement
class SameTypeFilter(val rdfType : String, variable : DynamicQueryVariable) extends QueryFilter{
  override def getSparql: String = {
    return getStatement(rdfType, variable).wherePhrase()
  }
}

object SameTypeFilter {
  def getStatement(rdfType : String, variable : DynamicQueryVariable): SimpleRDF = {
    return new SimpleRDF(variable, new StaticQueryVariable("w:P31"), new StaticQueryVariable(rdfType), true)
  }
}

