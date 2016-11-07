package query.specific

import query.specific.FindSubjectsWithProperty.getQuery
import query.{FindSubject, FindWhereStatement, MultipleGraphQuery}
import rdf.SimpleRDF

class FindSubjectsWithProperty(val property: String) extends MultipleGraphQuery(getQuery(property)) with FindSubject{


}

object FindSubjectsWithProperty extends FindWhereStatement{
  def getQuery(property : String) : String = {
    return whereStatement(new SimpleRDF(null, property, null))
  }
}




