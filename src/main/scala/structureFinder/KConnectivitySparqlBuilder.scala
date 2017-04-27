package structureFinder

import core.globals.MyDatasets
import core.query.Query


/**
  * Created by espen on 27.04.17.
  */
object KConnectivitySparqlBuilder {

  def main(args: Array[String]): Unit = {
//   findAllQueryPathsBetweenUpToLength("wd:Q2632", "wd:Q76", 4).foreach(pq => println(pq.queryString))
    println(findTopStructureBetweenEntities(List("http://www.wikidata.org/entity/Q76", "http://www.wikidata.org/entity/Q2632", "http://www.wikidata.org/entity/Q5")))
  }
  val pathMaxLength = 1


  case class Path(startEntity: String, endEntity: String, properties : Iterable[String], isSubjectList : Iterable[Boolean], middleEntities: Iterable[String])
  case class PathQuery(queryString : String, isSubjList: Iterable[Boolean], propertyVars : Iterable[String], middleEntityVars: Iterable[String]) {
  }

  //Length is defined as properties between start and end
  //Length = 1 = ?s ?p ?o
  //Length = 2 = ?s ?p ?m ?p2 ?o



  def findTopStructureBetweenEntities(entities: List[String]):Iterable[Path] = {
    val entityCombinations : List[(String, String)] = findCombinations(entities)
    val pathsFound = (for{
      (startEntity, endEntity) <- entityCombinations
      queryPaths = findAllQueryPathsBetweenUpToLength(startEntity, endEntity, pathMaxLength)
      query <- queryPaths
      paths = findPaths(query, startEntity, endEntity)
    }yield paths).flatten
    return pathsFound
  }
  def findPaths(query: PathQuery, startEntity: String, endEntity: String) : Iterable[Path] = {
    val queryExecutable = new Query(() => query.queryString, MyDatasets.dsWikidata)
    queryExecutable.execute()
    val propertyResults : Iterable[List[String]] = query.propertyVars.map(pVar => queryExecutable.getResults(pVar.tail) : List[String])
    val middleEntityResults : Iterable[List[String]] = query.middleEntityVars.map(mVar => queryExecutable.getResults(mVar.tail) : List[String])
    val numberOfPaths = propertyResults.head.length
    Range(0,numberOfPaths).map(index => Path(startEntity, endEntity, propertyResults.map(_(index)), query.isSubjList, middleEntityResults.map(_(index))))
  }
  def findCombinations(entities: List[String]): List[(String,String)] = {
    entities match {
      case _::Nil => Nil
      case head::tail => tail.map(e => (head, e)) ++ findCombinations(tail)
    }
  }


  def findAllQueryPathsBetweenUpToLength(startEntity: String, endEntity: String, length : Int) : List[PathQuery] = {
    val paths =  Range(1,length+1).foldLeft(Nil : List[PathQuery]){case (acc, next) => {
      println(s"finding paths of length $next")
      findPathsOfLength(startEntity, endEntity, next).toList ++ acc
      }
    }
    paths
  }


  private def findPathsOfLength(startEntity: String, endEntity: String, length: Int): Seq[PathQuery] = {
    val entityString = createEntityString(s"<$startEntity>", s"<$endEntity>")(length)
    val movesIsSubject = generateMovesOfLength(length)
    val queryStringsAndIsSubject = createQueryStrings(movesIsSubject, entityString)
    val propertyVars = Range(0, length).map(i => s"?p${i}")
    val middleEntityVars = Range(1, length).map(i => s"?m${i}")
    return queryStringsAndIsSubject.map { case (queryString, isSubjList) => PathQuery(queryString, isSubjList, propertyVars, middleEntityVars) }
  }

  private def createQueryStrings(movesIsSubject: List[List[Boolean]], entityString: String) : List[(String, List[Boolean])] = {
    val queryStringsAndIsSubjects = movesIsSubject.map(bList => {
      val entities = entityString.split(" ")
      (generateQueryPath(bList, entities), bList)
    })
    return queryStringsAndIsSubjects
  }



  private def generateQueryPath(bList: List[Boolean], entities : Array[String]) : String = {
    val queryString = bList.zipWithIndex.map { case (isSubject, index) => if (isSubject) s"${entities(index)} ?p$index ${entities(index + 1)}" else
      s"${entities(index + 1)} ?p$index ${entities(index)}"
    }.mkString(". ")
    s"select * where { $queryString }"
  }

  def createEntityString(startEntity : String = "?s", endEntity: String = "?o")(length : Int) : String = {
    val list = List(startEntity) ++ Range(1, length).map(i => s"?m$i") ++ List(endEntity)
    return list.mkString(" ")
  }



  def generateMovesOfLength(length: Int) : List[List[Boolean]] = {
    val starting = List(List(true), List(false))
    return generateMoves(starting, length -1)
  }

  def generateMoves(moveList : List[List[Boolean]], lengthLeft : Int) : List[List[Boolean]] = {
    lengthLeft match {
      case 0 => moveList
      case _ => val moveListUpdated = moveList.foldRight(Nil: List[List[Boolean]]) { (next, acc) =>
        val option1 = true :: next
        val option2 = false :: next
        option1 :: option2 :: acc
      }
        return generateMoves(moveListUpdated, lengthLeft - 1)
    }
  }

}
