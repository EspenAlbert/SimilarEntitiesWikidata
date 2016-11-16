package strategies
import feature.Feature
import rdf.GraphRDF

/**
  * Created by Espen on 11.11.2016.
  */
case class InANotInBStrategy(property: String, isSubject: Boolean, value : String) extends Strategy{
  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = ???

  override def findSimilars(): List[String] = ???

  override val weight: Double = ???
}
