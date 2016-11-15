package ownOntologyPopularizer

import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.CountAttributeGenerator

import scala.io.Source

/**
  * Created by Espen on 02.11.2016.
  */
class TestCountAttributeGenerator extends FunSuite{
  test("There should be no errors") {
    CountAttributeGenerator.generateCounts()
    assert(Source.fromFile("output/countForProperties.nt").getLines().length == 2398)
  }


}
