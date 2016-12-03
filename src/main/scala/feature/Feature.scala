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
    val comparedValue = floor(that.getScore() - this.getScore()).toInt
//    println(comparedValue, "for feature: ", featureType, " with count :", count, " and weight: ", weight, " for property: ", property)

    return comparedValue
  }

  override def toString: String = {
    return s"Feature for $property with $featureType had value: $count * $weight"
  }
}
