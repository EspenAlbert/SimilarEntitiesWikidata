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
    val compared = floor(that.getScore() - this.getScore()).toInt
//    println(comparedValue, "for feature: ", featureType, " with count :", count, " and weight: ", weight, " for property: ", property)
    try {
      assert(compared.isInstanceOf[Int])
      return compared
    } catch {
      case e: Throwable => println(s"failed too compare two features  $this and  $that"); return -1
    }
//    return compared
  }

  override def toString: String = {
    return s"Feature for $property with $featureType had value: $count * $weight"
  }
}
