package core.feature

import core.globals.FeatureType
import core.globals.FeatureType.FeatureType

/**
  * Created by espen on 17-Dec-16.
  */
class ExpandNodeFeature(property : String, path: String, featureType : FeatureType= FeatureType.searchExpandNode, count : Int=1, weight : Double=1) extends Feature(property, featureType, count, weight){

  override def toString: String = {
    path
  }

}
