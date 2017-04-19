package rdf

import core.globals.KnowledgeGraph
import core.rdf.{GraphRDF, GraphRDFDescriptivePropertyChecker}
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.ActiveTag

/**
  * Created by espen on 19.04.17.
  */
class TestGraphRDFDescriptivePropertyChecker extends FunSuite{
  test("Should be the same for ringo starr", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val rStarr= WikidataFactory.ringoStarr.id
    val ordinaryRDF = new GraphRDF(rStarr)
    val descriptiveGraphRDF = new GraphRDFDescriptivePropertyChecker(rStarr)
    assert(ordinaryRDF.statementsList.forall(triple => descriptiveGraphRDF.statementsList.contains(triple)))
    assert(descriptiveGraphRDF.statementsList.forall(triple => ordinaryRDF.statementsList.contains(triple)))
  }
  test("Should not be the same for UK, should not have country of citizenship property", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val uk= WikidataFactory.ringoStarr.countryOfCitizenShipValue
    val descriptiveGraphRDF = new GraphRDFDescriptivePropertyChecker(uk)
    assert(!descriptiveGraphRDF.statementsList.exists(_._2 == WikidataFactory.ringoStarr.countryOfCitizenShipProperty))
    assert(descriptiveGraphRDF.statementsList.exists(_._2 == WikidataFactory.headOfGovernment))

  }

}
