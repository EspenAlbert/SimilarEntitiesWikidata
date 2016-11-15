package rdf

import globals.PrimitiveDatatype
import query.filters.{NotEqualFilter, SameTypeFilter, StringLanguageFilter}
import query.variables._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 15.11.2016.
  */
object SimpleRDFFactory {

  private val isDynamicVariable = """\?[^\s]*\s?.*""".r

  private def createDynamicVariable(strings: Array[String]): ResultQueryVariable = {
    val countExist = strings.exists(_ == OptionsForResultQueryVariable.count.toString)
    val distinct = strings.exists( _ == OptionsForResultQueryVariable.distinct.toString)
    val name = strings.head
    val dynamicQueryVariable = DynamicQueryVariable(name.drop(1), distinct)
    for(filter <- strings.drop(1)) { //First value is the name
      if(filter.startsWith(OptionsForResultQueryVariable.sameTypeFilter.toString)) dynamicQueryVariable.addQueryFilter(new SameTypeFilter(filter.substring(filter.indexOf("_") + 1), dynamicQueryVariable))
      if(filter.startsWith(OptionsForResultQueryVariable.sameLanguageFilter.toString)) {
        val filterVariables = filter.split("_").drop(1) //First value is the filtername itself...
        dynamicQueryVariable.addQueryFilter(new StringLanguageFilter(dynamicQueryVariable, filterVariables(0), filterVariables(1)))
      }
      if(filter.startsWith(OptionsForResultQueryVariable.notEqualFilter.toString)) {
        val filterVariables = filter.split("_").drop(1) //First value is the filtername itself...
        dynamicQueryVariable.addQueryFilter(new NotEqualFilter(dynamicQueryVariable, filterVariables(0)))
      }
    }
    val variable = if(countExist) CountQueryVariable(name, distinct, dynamicQueryVariable) else dynamicQueryVariable
    return variable
  }

  private def createStaticVariable(t: String): QueryVariable = {
    if(!t.contains(" ")) {
      return StaticQueryVariable(t)
    }
    val Array(x, y) = t.split(" ")
    return StaticQueryVariable(x, PrimitiveDatatype.withName(y))
  }

  def getStatement(triple : Tuple3[String, String, String]) : SimpleRDF = {
    val arrayOfVariables = new ArrayBuffer[QueryVariable]()
    for(t <- List(triple._1, triple._2, triple._3)) {
      isDynamicVariable.findFirstIn(t) match {
        case Some(s) => arrayOfVariables.append(createDynamicVariable(s.split(" ")))
        case None => arrayOfVariables.append(createStaticVariable(t))
      }
    }
    return new SimpleRDF(arrayOfVariables(0), arrayOfVariables(1), arrayOfVariables(2))
  }
  def getResultVariables(statements : SimpleRDF*) :List[ResultQueryVariable] = {
    val listOfVariables = for(s <- statements)yield s.getResultVariables()
    val flatList = listOfVariables.flatten
    if(flatList.exists((s) => s.isInstanceOf[CountQueryVariable])) return flatList.filter(_.isInstanceOf[CountQueryVariable]).toList
    if(flatList.exists(isADistinctVariable(_))) return flatList.filter(isADistinctVariable(_)).toList
    return flatList.toList
  }

  def isADistinctVariable(variable : ResultQueryVariable) : Boolean = {
    variable match {
      case DynamicQueryVariable(p, true) => true
      case CountQueryVariable(p, true, o) => true
      case _ => false
    }
  }
}
