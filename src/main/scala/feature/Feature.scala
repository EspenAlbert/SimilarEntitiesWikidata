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
    return floor(that.getScore() - this.getScore()).toInt
  }
}
