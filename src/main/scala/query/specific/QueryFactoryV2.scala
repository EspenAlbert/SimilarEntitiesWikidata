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
    assert(resultQueryVariable.length == 1)
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

  def findDomainCount(property: String): Int = {
    val statement = SimpleRDFFactory.getStatement((property, SimilarPropertyOntology.domainCount.toString, "?o"))
    findSingleValue(statement)
  }
  def findRangeCount(property: String): Int = {
    val statement = SimpleRDFFactory.getStatement((property, SimilarPropertyOntology.rangeCount.toString, "?o"))
    findSingleValue(statement)
  }
  def getStrategies(prop: String) : List[String] = {
    val statement = SimpleRDFFactory.getStatement((prop, SimilarPropertyOntology.rdfType.toString, "?s"))
    val statement2 = SimpleRDFFactory.getStatement(("?s "+ OptionsForResultQueryVariable.ignoreMe,SimilarPropertyOntology.rdfsSubclassOf.toString, SimilarPropertyOntology.spoBaseStrategy.toString))
    return findList(statement, statement2)
  }


  def findAllDistinctProperties : List[String] = {
    val statement: SimpleRDF = SimpleRDFFactory.getStatement(("?s", "?p " + OptionsForResultQueryVariable.distinct, "?o"))
    findList(statement)
  }
  def findObjectsWithProperty(property: String): List[String] = {
    val statement = SimpleRDFFactory.getStatement(("?s", property, "?o" + OptionsForResultQueryVariable.distinct))
    findList(statement)
  }
  def findSubjectsWithProperty(property : String) : List[String] = {
    val statement = SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.distinct, property, "?o"))
    findList(statement)
  }
  def findSubjectsAndProperties(objectEntity : String) : Seq[List[String]] = {
    val statement = SimpleRDFFactory.getStatement(("?s", "?p", objectEntity))
    findMultipleLists(statement)
  }

  def findSubjectsOfType(rdfType : String) : List[String] = {
    val statment = SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.distinct.toString +" " + OptionsForResultQueryVariable.sameTypeFilter.toString + "_" + rdfType, "?p", "?o"))
    findList(statment)

  }
  def findPropertiesAndObjects(subjectEntity : String) : Seq[List[String]] = {
    val statement = SimpleRDFFactory.getStatement((subjectEntity, "?p", "?o"))
    findMultipleLists(statement)
  }
  def findTotalCountSubjectsWhereProperty(property : String, distinct : Boolean = false) : Int = {
    val statement = if(distinct) SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.distinct + " " + OptionsForResultQueryVariable.count, property, "?o " + variables.OptionsForResultQueryVariable.ignoreMe))
    else SimpleRDFFactory.getStatement(("?s " + OptionsForResultQueryVariable.count, property, "?o"))
    findSingleValue(statement)
  }
  def findTotalCountObjectsWhereProperty(property: String, distinct : Boolean = false) : Int = {
    val statement = if(distinct) SimpleRDFFactory.getStatement(("?s", property, "?o " + OptionsForResultQueryVariable.distinct + " " + OptionsForResultQueryVariable.count))
    else SimpleRDFFactory.getStatement(("?s", property, "?o " + OptionsForResultQueryVariable.count))
    findSingleValue(statement)
  }

  def findAllPropertiesOfCustomClass(customPropertyClass: CustomPropertyClass) : List[String] = {
    val statment = SimpleRDFFactory.getStatement(("?s", SimilarPropertyOntology.rdfType.toString, customPropertyClass.toString))
    findList(statment)
  }
//  def ask(statements: SimpleRDF*) : Boolean = {
//    val filter = new WhereFilter(statements: _*)
//    return AskQuery.ask(() => s"ask {${filter.getWhereClause()} }", dataset)
//  }
}
