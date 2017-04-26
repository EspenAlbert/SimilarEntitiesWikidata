package core.feature

import core.globals.FeatureType
import core.globals.FeatureType.FeatureType

/**
  * Created by espen on 17-Dec-16.
  */
class PathFeature(property : String, val path: String, featureType : FeatureType= FeatureType.searchExpandNode, count : Int=1, weight : Double=1) extends Feature(property, featureType, count, weight){

  override def toString: String = {
    path
  }

}

object PathFeature {
  def createPathLength2(qEntity: String, isSubject: Boolean, property: String, middleNode: String, middleNodeIsSubj: Boolean, property2: String, foundEntity: String): String = {
    createPathLength1(qEntity, property, isSubject, middleNode) + s",$middleNodeIsSubj,$property2,$foundEntity"
  }

  def createPathLength1(qEntity : String, property : String, isSubject : Boolean, foundEntity : String): String = {
    s"$qEntity,$isSubject,$property,$foundEntity"
  }

}
