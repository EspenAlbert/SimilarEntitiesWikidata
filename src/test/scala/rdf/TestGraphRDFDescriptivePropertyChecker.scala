package rdf

import core.globals.KnowledgeGraph
import core.rdf.{GraphRDF, GraphRDFDescriptivePropertyChecker}
import data.WikidataFactory
import org.scalatest.FunSuite
import similarityFinder.MyConfiguration
import tags.ActiveTag

/**
  * Created by espen on 19.04.17.
  */
class TestGraphRDFDescriptivePropertyChecker extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  private val wd = WikidataFactory
  val rStarr= wd.ringoStarr
  test("Should be the same for ringo starr", ActiveTag) {
    val ordinaryRDF = new GraphRDF(rStarr.id)
    val descriptiveGraphRDF = new GraphRDFDescriptivePropertyChecker(rStarr.id)
    assert(ordinaryRDF.statementsList.forall(triple => descriptiveGraphRDF.statementsList.contains(triple)))
    assert(descriptiveGraphRDF.statementsList.forall(triple => ordinaryRDF.statementsList.contains(triple)))
  }
  test("Should not be the same for UK, should not have country of citizenship property", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val uk= wd.ringoStarr.countryOfCitizenShipValue
    val descriptiveGraphRDF = new GraphRDFDescriptivePropertyChecker(uk)
    assert(!descriptiveGraphRDF.statementsList.exists(_._2 == wd.ringoStarr.countryOfCitizenShipProperty))
    assert(descriptiveGraphRDF.statementsList.exists(_._2 == wd.headOfGovernment))

  }
  test("Human and band should be among the top comparable types for ringoStarr", ActiveTag) {
    MyConfiguration.filterOnRdfType = true
    val graphRDF = new GraphRDFDescriptivePropertyChecker(rStarr.id)
    assert(graphRDF.getTypes.contains(wd.human))
    assert(graphRDF.getTypes.contains(wd.band), "Didn't contain rock band among comparable types!")
  }

}
