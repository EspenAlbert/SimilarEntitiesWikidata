package rdf

import globals.SimilarPropertyOntology
import org.scalatest.{BeforeAndAfter, FunSuite}

/**
  * Created by Espen on 02.11.2016.
  */
class TestGraphRDF extends FunSuite with BeforeAndAfter{

  var graph: GraphRDF = _
  before {
    graph = new GraphRDF(SimilarPropertyOntology.w.toString() +"Q76")
  }

  test("obama should have 545 statements") {
    assert(graph.statements.toList.length == 545)
  }
  test("get properties for obama") {
    assert(graph.getUniqueWikidataPropertiesWithoutTheMostCommon().exists(_ == SimilarPropertyOntology.w.toString + "P31"))
    assert(!graph.getUniqueWikidataPropertiesWithoutTheMostCommon().exists(_.contains("Q")))
  }
  test("get properties where obama is subject") {
    assert(graph.getProperties(true).length == 135)
  }
  test("get properties where obama is object") {
    assert(graph.getProperties(o = true).length == 410)
  }
  test("scaling factor for the same entity must be 1") {
    assert(graph.findScalingFactor(graph) == 1)
  }
  test("scaling factor for a similar entity works as expected") {
    val scalingFactor = graph.findScalingFactor(new GraphRDF("w:Q6294"))
    print(scalingFactor)
    assert(scalingFactor > 0.5)
  }


}
