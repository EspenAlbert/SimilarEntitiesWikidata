package strategies
import breeze.numerics.log
import feature.Feature
import globals.{MyDatasets, SimilarPropertyOntology}
import query.specific.QueryFactory
import query.variables.{DynamicQueryVariable, StaticQueryVariable}
import rdf.SimpleRDF

/**
  * Created by Espen on 11.11.2016.
  */
case class DirectLinkStrategy(property: String, isSubject: Boolean, entity : String) extends Strategy{
  private val directLinks = new DynamicQueryVariable("o", false)
  private val findOthers: SimpleRDF =  if(isSubject) new SimpleRDF(new StaticQueryVariable(entity), new StaticQueryVariable(property), directLinks) else
    new SimpleRDF(s = directLinks, p = new StaticQueryVariable(property), o = new StaticQueryVariable(entity))
  override def execute(otherEntities: List[String]): Map[String, List[Feature]] = ???

  private var similars : List[String] = List[String]()

  override def findSimilars(): List[String] = {
    QueryFactory.dataset = MyDatasets.Wikidata
    similars =  QueryFactory.findEntities(findOthers, directLinks)
    return similars
  }

  override def weightCalculator(): Double = log(SimilarPropertyOntology.maxCountForProperties.toString.toInt / similars.length)

  override def getWeight(f: () => Double): Double = {
    return weightCalculator()
  }
}
