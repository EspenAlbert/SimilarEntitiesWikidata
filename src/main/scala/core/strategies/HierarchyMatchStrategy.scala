package core.strategies
import core.feature.Feature
import core.rdf.GraphRDF

/**
  * Created by Espen on 11.11.2016.
  */
case class HierarchyMatchStrategy(property: String, isSubject: Boolean) extends Strategy{
  override def execute(otherEntities: List[GraphRDF]): Map[String, Feature] = ???

  override def findSimilars(): List[String] = ???

  override val weight: Double = ???
}
