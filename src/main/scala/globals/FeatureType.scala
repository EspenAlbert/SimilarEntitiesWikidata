package globals

/**
  * Created by Espen on 09.11.2016.
  */
object FeatureType extends Enumeration{
  type FeatureType = Value
  val valueMatch = Value("Value match")
  val sameProperty = Value("Same property")
  val inANotInB = Value("In a not in b")


}
