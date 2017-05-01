package core.feature

import core.globals.FeatureType
import core.globals.FeatureType.FeatureType
import structureFinder.KConnectivitySparqlBuilder
import structureFinder.KConnectivitySparqlBuilder.Path

/**
  * Created by espen on 17-Dec-16.
  */
class PathFeature(property : String, val path: String, featureType : FeatureType= FeatureType.searchExpandNode, count : Int=1, weight : Double=1) extends Feature(property, featureType, count, weight){

  override def toString: String = {
    path
  }

}

object PathFeature {
  def createpath(path: Path): String = {
    val Path(startE, endE, props, isSubjs, middleEs) = path
    val pathLength = path.properties.size
    Range(0, pathLength).map(_ match {
      case 0 if pathLength == 1 => createPathLength1(startE, props.head, isSubjs.head, endE)
      case 0 if pathLength > 1 => createPathLength1(path.startEntity, path.properties.head, path.isSubjectList.head, path.middleEntities.head)
      case a if pathLength == a+1 => s"${isSubjs(a)},${props(a)},$endE"
      case a if pathLength > a+1 => s"${isSubjs(a)},${props(a)},${middleEs(a)}"
    })
    .mkString(",")
  }

  def createPathLength2(qEntity: String, isSubject: Boolean, property: String, middleNode: String, middleNodeIsSubj: Boolean, property2: String, foundEntity: String): String = {
    createPathLength1(qEntity, property, isSubject, middleNode) + s",$middleNodeIsSubj,$property2,$foundEntity"
  }

  def createPathLength1(qEntity : String, property : String, isSubject : Boolean, foundEntity : String): String = {
    s"$qEntity,$isSubject,$property,$foundEntity"
  }

}
