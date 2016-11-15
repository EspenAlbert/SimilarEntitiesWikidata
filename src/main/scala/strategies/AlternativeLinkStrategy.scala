package strategies
import feature.Feature

/**
  * Created by Espen on 11.11.2016.
  */
case class AlternativeLinkStrategy(property: String, isSubject: Boolean) extends Strategy{
  override def execute(otherEntities: List[String]): Map[String, List[Feature]] = ???

  override def findSimilars(): List[String] = ???

  override def weightCalculator(): Double = ???

  override def getWeight(f: () => Double): Double = ???
}
