package rdf

import globals.Namespace
import org.scalatest.{BeforeAndAfter, FunSuite}

/**
  * Created by Espen on 02.11.2016.
  */
class TestGraphRDF extends FunSuite with BeforeAndAfter{

  var graph: GraphRDF = _
  before {
    graph = new GraphRDF(Namespace.w.toString() +"Q76")
  }

  test("obama should have 545 statements") {
    assert(graph.statements.toList.length == 545)
  }
  test("get properties for obama") {
    assert(graph.getProperties().exists(_ == Namespace.w.toString + "P31"))
    assert(!graph.getProperties().exists(_.contains("Q")))
  }
  test("get properties where obama is subject") {
    assert(graph.getProperties(true).length == 135)
  }
  test("get properties where obama is object") {
    assert(graph.getProperties(o = true).length == 410)
  }


}
