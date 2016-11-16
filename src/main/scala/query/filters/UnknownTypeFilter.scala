package query.filters

import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.{SimpleRDF, SimpleRDFFactory}

/**
  * Created by Espen on 07.11.2016.
  */
import query.filters.UnknownTypeFilter.getStatement

class UnknownTypeFilter(val rdfType : String, variable : DynamicQueryVariable) extends QueryFilter{
  override def getSparql: String = {
    return getStatement(rdfType, variable).wherePhrase()
  }
}

object UnknownTypeFilter {
  def getStatement(rdfType : String, variable : DynamicQueryVariable): SimpleRDF = {
    return new SimpleRDF(variable, new StaticQueryVariable("w:P31"), new DynamicQueryVariable(rdfType, false, true), true)
  }
}



