package strategies
import feature.Feature

/**
  * Created by Espen on 11.11.2016.
  */
case class ValueMatchStrategy(property: String, isSubject: Boolean, value : String) extends Strategy{
  override def execute(otherEntities: List[String]): Map[String, List[Feature]] = ???

  override def findSimilars(): List[String] = ???

  override def weightCalculator(): Double = ???

  override def getWeight(f: () => Double): Double = ???
}
