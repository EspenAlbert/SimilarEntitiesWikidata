package globals

/**
  * Created by Espen on 09.11.2016.
  */
object FeatureType extends Enumeration{


  type FeatureType = Value
  val inBNotInAGlobal = Value("In b not in a global")
  val timeComparison = Value("Time comparison")
  val alternativeMatch = Value("Alternative match")
  val valueMatch = Value("Value match")
  val sameProperty = Value("Same property")
  val inANotInB = Value("In a not in b")
  val inBNotInA = Value("In b not in a")
  val directLinkMatch = Value("Direct link match")

  implicit def getStringValue(featureType: FeatureType) : String = {
    return featureType.toString
  }

}
