package core.testData

import core.globals.KnowledgeGraphs
import core.rdf.GraphRDF
import org.scalatest.FunSuite

/**
  * Created by espen on 31.05.17.
  */
class IOFactoryTest extends FunSuite {

  implicit val knowledgeGraph = KnowledgeGraphs.wikidata
  val wd = WikidataFactory

  test("testGetRangeCounts") {
    val rangeCounts = IOFactory.getRangeCounts
    val gRdftRStarr = new GraphRDF(wd.ringoStarr.id)
    val props = gRdftRStarr.statementsList.filter(_._3 == wd.ringoStarr.id).map(_._2)
    assert(props.forall(rangeCounts.contains))
  }
  test("testGetDomainCounts") {
    val domainCounts = IOFactory.getDomainCounts
    val gRdftRStarr = new GraphRDF(wd.ringoStarr.id)
    val props = gRdftRStarr.statementsList.map(_._2)
    assert(props.forall(domainCounts.contains))
    println(domainCounts.size)
  }

}
