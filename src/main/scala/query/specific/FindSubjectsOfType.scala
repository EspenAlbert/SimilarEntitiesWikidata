package query.specific

import query.specific.FindSubjectsOfType.getQuery
import query.{FindSubject, FindWhereStatement, MultipleGraphQuery}
import rdf.SimpleRDF

class FindSubjectsOfType(val typeName: String) extends MultipleGraphQuery(getQuery(typeName)) with FindSubject{


}

object FindSubjectsOfType extends FindWhereStatement{
  def getQuery(typeName : String) : String = {
    return whereStatement(new SimpleRDF(null, "w:P31", typeName))
  }
}


