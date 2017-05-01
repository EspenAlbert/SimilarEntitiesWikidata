package structureFinder

import core.feature.PathFeature
import core.globals.{KnowledgeGraph, MyDatasets}
import core.query.Query
import structureFinder.pathsBetweenEntities.{PathRanker, QueryPathBuilder, QueryPathExecutor}



/**
  * Created by espen on 27.04.17.
  */
object KConnectivitySparqlBuilder {
  //Length is defined as properties between start and end
  //Length = 1 = ?s ?p ?o
  //Length = 2 = ?s ?p ?m ?p2 ?o
  var pathMaxLength = 2
  case class Path(startEntity: String, endEntity: String, properties : List[String], isSubjectList : List[Boolean], middleEntities: List[String]) {
    require(properties.length == isSubjectList.length, s"failed to create path, properties and isSubjectList must be of same length!: ${properties} ${isSubjectList} ${middleEntities}")
    require(isSubjectList.length == middleEntities.length+1, s"failed to create path, middleEntities should be shorter than properties: ${properties} ${isSubjectList} ${middleEntities}")
    override def toString: String = {
      PathFeature.createpath(this)
    }
  }
  case class PathQuery(queryString : String, isSubjList: Iterable[Boolean], propertyVars : Iterable[String], middleEntityVars: Iterable[String]) {
  }

  def findTopStructureBetweenEntities(entities: List[String]):Iterable[Path] = {
    val entityCombinations: List[(String, String)] = findCombinations(entities)
    val pathsFound = (for {
      (startEntity, endEntity) <- entityCombinations
      queryPaths = QueryPathBuilder.findAllQueryPathsBetweenUpToLength(startEntity, endEntity, pathMaxLength)
      query <- queryPaths
      paths = QueryPathExecutor.findPaths(query, startEntity, endEntity)
    } yield paths).flatten
    //    return pathsFound
    implicit val kg = KnowledgeGraph.findKnowledgeGraphFromId(entities.head)
    val rankedPaths = PathRanker.rankOnPropertyRareness(pathsFound, pathsFound.length)
    val entitiesAsSet = entities.toSet
    rankedPaths.foldLeft((Nil: List[Path], Set[String]())) { (acc, next) => {
      (acc._2.contains(next.startEntity), acc._2.contains(next.endEntity)) match {
        case (a, b) if !a || !b =>
          val setOfFoundEntities = acc._2 ++ Set(next.startEntity, next.endEntity)
          if (setOfFoundEntities.size == entitiesAsSet.size) return next :: acc._1
          (next :: acc._1, setOfFoundEntities)
        case _ => acc
      }
    }
    }
    throw new Exception("Unable to find paths connecting all the entities...")

  }
   def findCombinations(entities: List[String]): List[(String,String)] = {
    entities match {
      case _::Nil => Nil
      case head::tail => tail.map(e => (head, e)) ++ findCombinations(tail)
    }
  }
  def findPathsBetweenEntities(entities: List[String]):List[String] = {
    val entityCombinations: List[(String, String)] = findCombinations(entities)
    val pathsFound = (for {
      (startEntity, endEntity) <- entityCombinations
      queryPaths = QueryPathBuilder.findAllQueryPathsBetweenUpToLength(startEntity, endEntity, pathMaxLength)
      query <- queryPaths
      paths = QueryPathExecutor.findPaths(query, startEntity, endEntity)
    } yield paths).flatten
    return pathsFound.map(_.toString)

  }




}
