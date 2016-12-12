package feature

import breeze.numerics.floor
import globals.FeatureType.FeatureType

/**
  * Created by Espen on 09.11.2016.
  */
class Feature(property : String, featureType : FeatureType, count : Int, weight : Double) extends Ordered[Feature]{
  def getScore() : Double = {
    return count * weight
  }

  override def compare(that: Feature): Int = {
    val compared = that.getScore() - this.getScore()
    if(compared > 0) return 1 //Other has higher score
    if(compared < 0) return -1 //This has higher score
    return 0
  }

  override def toString: String = {
    return s"Feature for $property with $featureType had value: $count * $weight"
  }
}
