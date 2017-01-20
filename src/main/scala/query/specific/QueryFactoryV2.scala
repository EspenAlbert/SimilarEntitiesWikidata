package query.specific

import globals.SimilarPropertyOntology
import ownOntologyPopularizer.CustomPropertyClass.CustomPropertyClass
import query.{variables, _}
import query.variables.{OptionsForResultQueryVariable, _}
import rdf.{SimpleRDF, SimpleRDFFactory}

/**
  * Created by Espen on 07.11.2016.
  */
object QueryFactoryV2 {

  def getQuery(statements: Seq[SimpleRDF]): Query = {
    val filter = new WhereFilter(statements : _*)
    val queryGenerator: () => String = () => filter.getSelect() + filter.getWhereClause()
    return new MultipleGraphQuery(queryGenerator, DatasetInferrer.getDataset(queryGenerator()))
  }

  def findList(statements : SimpleRDF*) : List[ResultVariable] = {
    val (query: Query, resultQueryVariable: List[ResultQueryVariable]) = getQueryAndResultVariables(statements: _*)
    assert(resultQueryVariable.length == 1 || resultQueryVariable.foldRight(true){(next: ResultQueryVariable, previousEqual: Boolean) => previousEqual && next == resultQueryVariable(0)})
    return getListFromQueryAndVariables(query, resultQueryVariable : _*)(0)
  }

  private def getQueryAndResultVariables(statements: SimpleRDF*): (Query, List[ResultQueryVariable]) = {
    val query: Query = getQuery(statements)
    val resultQueryVariable = SimpleRDFFactory.getResultVariables(statements: _*)
    (query, resultQueryVariable)
  }

  def findMultipleLists(statements : SimpleRDF*) : Seq[List[ResultVariable]] = {
    val (query: Query, resultQueryVariables: List[ResultQueryVariable]) = getQueryAndResultVariables(statements: _*)
    assert(resultQueryVariables.length > 1)
    return getListFromQueryAndVariables(query, resultQueryVariables: _*)
  }
  def findSingleValue(statements : SimpleRDF*) : ResultVariable = {
    val (query: Query, resultQueryVariables: List[ResultQueryVariable]) = getQueryAndResultVariables(statements: _*)
    assert(resultQueryVariables.length == 1)
    return getListFromQueryAndVariables(query, resultQueryVariables : _*)(0)(0)
  }

  def getListFromQueryAndVariables(query: Query, variables: ResultQueryVariable*) : Seq[List[ResultVariable]] = {
    query.execute()
    return for(variable <- variables) yield query.getResults(variable)
  }


//  def ask(statements: SimpleRDF*) : Boolean = {
//    val filter = new WhereFilter(statements: _*)
//    return AskQuery.ask(() => s"ask {${filter.getWhereClause()} }", dataset)
//  }
}
