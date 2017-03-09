package structureFinder

import core.query.MultipleGraphQuery
import core.query.specific.DatasetInferrer
import core.rdf.GraphRDF

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by espen on 23.01.17.
  */
object StructureFinder {

  def aEntityIsObject(entityList: List[String], objectValue : String) : Boolean = {
    return entityList.exists(_ == objectValue)
  }

  def replaceOrNot(index: Int, value: String) :String = {
    if(index > -1) s"?$index" else value
  }

  def replaceEntitiesWithIndex(statement: (String, String, String), entities: List[String]) : List[String]= {
    return statement.productIterator.asInstanceOf[Iterator[String]].map((s) => replaceOrNot(entities.indexOf(s), s)).toList
  }

  def generateQuery(listOfIndexStatements: ListBuffer[List[String]], numberOfEntities : Int) : String = {
    val selectVariables = for(
      i <- 0 until numberOfEntities)
      yield s"?$i"
    val select = "select " + selectVariables.mkString(" ")
    var query = ListBuffer[String](select, "where {")
    val usedIndexes = mutable.Set[String]()
    for(a <- listOfIndexStatements) {
      a match {
        case subj :: b :: c :: nil if !(usedIndexes.contains(subj) && usedIndexes.contains(c)) => {
          usedIndexes.add(subj)
          usedIndexes.add(c)
          query.append(subj + s"<$b>" + c + " .")
          println(a, b, c)
        }
        case d => println(d)
      }
    }
    query.append("}")
    return query.mkString("\n")
  }

  def findStructures(entities : List[String]): Unit = {
    val entityGraphs = entities.map(new GraphRDF(_))
    val directLinkMatches = ListBuffer[(String, String, String)]()

    for(e <- entityGraphs) {
      val otherEntities = entities.filterNot(e.entity == _)
      directLinkMatches ++= e.statementsList.filter((statement) => aEntityIsObject(otherEntities, statement._3))
    }
    directLinkMatches.foreach((s) => println(s.productIterator.asInstanceOf[Iterator[String]].toList.mkString(" ")))
    directLinkMatches.foreach((s) => println(replaceEntitiesWithIndex(s, entities)))
    val listOfIndexStatements = directLinkMatches.map((s) => replaceEntitiesWithIndex(s, entities))
    val queryString = generateQuery(listOfIndexStatements, entities.length)
    val query = new MultipleGraphQuery(() => queryString, DatasetInferrer.getDataset(queryString))
    println(query.getQuery())
    query.execute()
    println(query.getResultStream().toString())

  }

}
