package query.specific

import globals.{MyDatasets, SimilarPropertyOntology}
import ownOntologyPopularizer.CustomPropertyClass.CustomPropertyClass
import query._
import query.filters.SameTypeFilter
import query.variables._
import rdf.SimpleRDF

/**
  * Created by Espen on 07.11.2016.
  */
object QueryFactory {


  val subjects = new DynamicQueryVariable("s", false)
  val objects = new DynamicQueryVariable("o", false)
  val properties = new DynamicQueryVariable("p", false)
  var dataset = MyDatasets.Wikidata

  def getListFromQueryAndVariables(query: Query, variables: ResultQueryVariable*) : Seq[List[ResultVariable]] = {
    query.execute()
    return for(variable <- variables) yield query.getResults(variable)
  }
  def findEntities(findOthers: SimpleRDF, directLinks: DynamicQueryVariable): List[String] = {
    val filter = new WhereFilter(findOthers)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, directLinks)(0)
  }
  def findDomainCount(property: String): Int = {
    dataset = MyDatasets.SimilarProperties
    findIntVariableValue(new SimpleRDF(new StaticQueryVariable(property), new StaticQueryVariable(SimilarPropertyOntology.domainCount.toString), objects), objects)
  }
  def findRangeCount(property: String): Int = {
    dataset = MyDatasets.SimilarProperties
    findIntVariableValue(new SimpleRDF(new StaticQueryVariable(property), new StaticQueryVariable(SimilarPropertyOntology.rangeCount.toString), objects), objects)
  }
  def getStrategies(prop: String) : List[String] = {
    val statement = new SimpleRDF(new StaticQueryVariable(prop), new StaticQueryVariable(SimilarPropertyOntology.rdfType.toString), subjects)
    val statement2 = new SimpleRDF(subjects, new StaticQueryVariable(SimilarPropertyOntology.rdfsSubclassOf.toString), new StaticQueryVariable(SimilarPropertyOntology.spoBaseStrategy.toString))
    val whereFilter = new WhereFilter(statement, statement2)
    val query = new Query(() => whereFilter.getSelect() + whereFilter.getWhereClause(), MyDatasets.SimilarProperties)
    return getListFromQueryAndVariables(query, subjects)(0)
  }
  def findStringWhere(statement: SimpleRDF, resultVariable : DynamicQueryVariable): Option[String] = {
    val filter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    try {
      return Some(getListFromQueryAndVariables(query, resultVariable)(0)(0))
    } catch {
      case s : IndexOutOfBoundsException => return None
    }
  }

  def findIntVariableValue(statement: SimpleRDF, resultVariable: DynamicQueryVariable) : Int = {
    val filter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, resultVariable)(0)(0)
  }
  def findAllDistinctProperties : List[String] = {
    val properties = new DynamicQueryVariable("p", true)
    val statement: SimpleRDF = new SimpleRDF(p= properties)
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, properties)(0)
  }
  def findObjectsWithProperty(property: String): List[String] = {
    val objects = new DynamicQueryVariable("o", true)
    val statement = new SimpleRDF(o = objects, p = new StaticQueryVariable(property))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, objects)(0)
  }
  def findSubjectsWithProperty(property : String) : List[String] = {
    val subjects = new DynamicQueryVariable("s", true)
    val statement = new SimpleRDF(s = subjects, p = new StaticQueryVariable(property))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, subjects)(0)
  }
  def findSubjectsAndProperties(objectEntity : String) : Seq[List[String]] = {
    val statement = new SimpleRDF(s = subjects, p = properties, o = new StaticQueryVariable(objectEntity))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, subjects, properties)
  }
  def findAllStatementsForSubjectsOfType(rdfType : String) : Seq[List[String]] = {
    val subjects: DynamicQueryVariable = new DynamicQueryVariable("s", false)
    val statement = new SimpleRDF(s = subjects, p = properties, o = objects)
    subjects.addQueryFilter(new SameTypeFilter(rdfType, subjects))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, subjects, properties, objects)
  }
  def findSubjectsOfType(rdfType : String) : List[String] = {
    val filter = new WhereFilter(SameTypeFilter.getStatement(rdfType, subjects))
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, subjects)(0)

  }
  def findPropertiesAndObjects(subjectEntity : String) : Seq[List[String]] = {
    val statement = new SimpleRDF(s = new StaticQueryVariable(subjectEntity), p = properties, o = objects)
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, properties, objects)
  }
  def findTotalCountSubjectsWhereProperty(property : String, distinct : Boolean = false) : Int = {
    val statement = new SimpleRDF(s = subjects, p = new StaticQueryVariable(property))
    val count = new CountQueryVariable("c", distinct, subjects)
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => count.getSelectPhrase + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, count)(0)(0).toInt
  }
  def findTotalCountObjectsWhereProperty(property: String, distinct : Boolean = false) : Int = {
    val statement = new SimpleRDF(o = objects, p = new StaticQueryVariable(property))
    val count = new CountQueryVariable("c", distinct, objects)
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => count.getSelectPhrase + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, count)(0)(0).toInt
  }

  def findAllPropertiesOfCustomClass(customPropertyClass: CustomPropertyClass) : List[String] = {
    val statement = new SimpleRDF(s = subjects, p = new StaticQueryVariable(SimilarPropertyOntology.rdfType.toString), o = new StaticQueryVariable(customPropertyClass.toString))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new Query(() => filter.getSelect() +  filter.getWhereClause(), dataset = MyDatasets.SimilarProperties)
    return getListFromQueryAndVariables(query, subjects)(0)
  }
  def ask(statements: SimpleRDF*) : Boolean = {
    val filter = new WhereFilter(statements: _*)
    return AskQuery.ask(() => s"ask {${filter.getWhereClause()} }", dataset)
  }
}
