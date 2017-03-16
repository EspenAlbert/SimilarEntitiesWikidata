package implicitTest

import core.globals.{MyDatasets, SimilarPropertyOntology}
import org.scalatest.FunSuite
import preprocessing.ownOntologyPopularizer.MapPropertiesToPropTypes
import similarityFinder.implicitTest.SimFinderDS
import tags.{Active, FastTag}

/**
  * Created by espen on 10.03.17.
  */

class TestImplicit extends FunSuite{

  test("Implicit works", FastTag) {
    val a = new SimFinderDS(MyDatasets.Wikidata)
    val b = new SimFinderDS(MyDatasets.SimilarProperties)
    val c = new SimFinderDS(MyDatasets.DsBig)
    a.worksForFunctionToo()
    b.worksForFunctionToo()
    c.worksForFunctionToo()
    a.worksForCallingAnotherFunctionFromFunction("wannabe")
    a.worksForCallingAnotherFunctionFromFunction(SimilarPropertyOntology.valueMatchClass)
    b.worksForCallingAnotherFunctionFromFunction(SimilarPropertyOntology.valueMatchClass)
    c.worksForCallingAnotherFunctionFromFunction(SimilarPropertyOntology.valueMatchClass)
  }
  test("Implicit overriding a value", Active) {
    val dummyList = List("a")
    implicit val dataset = MyDatasets.DsBig
    assertResult(List(dataset)){SimFinderDS.filterGeoPropertyTypes(dummyList)}
    assertResult(List(MyDatasets.ValueMatch)){SimFinderDS.filterGeoPropertyTypes2(dummyList)}
  }

}
