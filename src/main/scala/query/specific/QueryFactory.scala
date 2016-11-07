package query.specific

import globals.{MyDatasets, Namespace}
import query.filters.{SameTypeFilter, StringLanguageFilter}
import query.variables.{CountQueryVariable, DynamicQueryVariable, ResultVariable, StaticQueryVariable}
import query.{MultipleGraphQuery, NamedGraphQuery, Query, WhereFilter}
import rdf.SimpleRDF

/**
  * Created by Espen on 07.11.2016.
  */
object QueryFactory {
  val subjects = new DynamicQueryVariable("s", false)
  val objects = new DynamicQueryVariable("o", false)
  val properties = new DynamicQueryVariable("p", false)
  var dataset = MyDatasets.Wikidata

  def getListFromQueryAndVariables(query: Query, variables: ResultVariable*) : Seq[List[String]] = {
    query.execute()
    return for(variable <- variables) yield query.getResults(variable)
  }

  def findAllDistinctProperties : List[String] = {
    val properties = new DynamicQueryVariable("p", true)
    val statement: SimpleRDF = new SimpleRDF(p= properties)
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, properties)(0)
  }
  def findSubjectsWithProperty(property : String) : List[String] = {
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
  def findSubjectsOfType(rdfType : String) : List[String] = {
    val subjects: DynamicQueryVariable = new DynamicQueryVariable("s", false)
    val statement = new SimpleRDF(s = subjects)
    subjects.addQueryFilter(new SameTypeFilter(rdfType, subjects))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, subjects)(0)
  }
  def findPropertiesAndObjects(subjectEntity : String) : Seq[List[String]] = {
    val statement = new SimpleRDF(s = new StaticQueryVariable(subjectEntity), p = properties, o = objects)
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, properties, objects)
  }
  def findTotalCountWhereProperty(property : String) : Int = {
    val statement = new SimpleRDF(s = subjects, p = new StaticQueryVariable(property))
    val count = new CountQueryVariable("c", false, subjects)
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new Query(() => count.getSelectPhrase + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, count)(0)(0).toInt
  }
  def findIDForPropertyLabelQuery(propertyClassLabel : String) : String = {
    val objects = new DynamicQueryVariable("o", false)
    val statement = new SimpleRDF(s = subjects, o = objects)
    objects.addQueryFilter(new StringLanguageFilter(objects, propertyClassLabel))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new MultipleGraphQuery(() => filter.getSelect() + filter.getWhereClause(), dataset)
    return getListFromQueryAndVariables(query, subjects)(0)(0)
  }
  def findAllPropertiesOfCustomClass() : List[String] = {
    val statement = new SimpleRDF(s = subjects, p = new StaticQueryVariable(Namespace.rdfType.toString), o = new StaticQueryVariable(Namespace.basePropertyClassId.toString))
    val filter: WhereFilter = new WhereFilter(statement)
    val query = new Query(() => filter.getSelect() + NamedGraphQuery.getFrom(Namespace.spo.toString)+ filter.getWhereClause(), dataset = MyDatasets.SimilarProperties)
    return getListFromQueryAndVariables(query, subjects)(0)

  }
}
