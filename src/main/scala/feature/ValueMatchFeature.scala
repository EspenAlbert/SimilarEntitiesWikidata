package feature

import globals.FeatureType.FeatureType

/**
  * Created by espen on 17-Dec-16.
  */
class ValueMatchFeature(property : String, featureType : FeatureType, count : Int, weight : Double, valueMatchId : String) extends Feature(property, featureType, count, weight){

  override def toString: String = {
    super.toString + s" value match entity: $valueMatchId" + Feature.findLabel(valueMatchId)
  }

}
