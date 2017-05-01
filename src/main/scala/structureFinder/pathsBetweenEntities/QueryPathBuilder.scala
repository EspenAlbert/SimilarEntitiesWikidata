package structureFinder.pathsBetweenEntities

import structureFinder.KConnectivitySparqlBuilder.PathQuery

/**
  * Created by espen on 28.04.17.
  */
object QueryPathBuilder {
  def findAllQueryPathsBetweenUpToLength(startEntity: String, endEntity: String, length : Int) : List[PathQuery] = {
    val paths =  Range(1,length+1).foldLeft(Nil : List[PathQuery]){
      case (acc, next) => {
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


  def generateFilter(entities: Array[String]): String = {
    entities.map(e => s"filter(isUri($e))").mkString("\n")
  }

  private def generateQueryPath(bList: List[Boolean], entities : Array[String]) : String = {
    val queryString = bList.zipWithIndex.map { case (isSubject, index) => if (isSubject) s"${entities(index)} ?p$index ${entities(index + 1)}" else
      s"${entities(index + 1)} ?p$index ${entities(index)}"
    }.mkString(". ")
    s"select * where { $queryString ${generateFilter(entities.tail.init)} } "
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
