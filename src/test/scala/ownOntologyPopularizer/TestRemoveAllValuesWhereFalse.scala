package ownOntologyPopularizer

import org.scalatest.FunSuite
import ownOntologyPopularizer.attributesGenerator.RemoveAllValuesWhereFalse

import scala.io.Source

/**
  * Created by Espen on 16.11.2016.
  */
class TestRemoveAllValuesWhereFalse extends FunSuite{
  test("lines is removed from sharable domain") {
    val before = Source.fromFile("output/sharableDomain.nt").getLines().length
    RemoveAllValuesWhereFalse.removeBooleansWhereFalse("sharableDomain")
    val after = Source.fromFile("output/sharableDomain.nt").getLines().length
    assert(after < before)

  }
  test("lines is removed from sharable range") {
    val before = Source.fromFile("output/sharableRange.nt").getLines().length
    RemoveAllValuesWhereFalse.removeBooleansWhereFalse("sharableRange")
    val after = Source.fromFile("output/sharableRange.nt").getLines().length
    assert(after < before)
  }
  test("lines is removed from sameTypePossible ") {
    val before = Source.fromFile("output/sameTypePossible.nt").getLines().length
    RemoveAllValuesWhereFalse.removeBooleansWhereFalse("sameTypePossible")
    val after = Source.fromFile("output/sameTypePossible.nt").getLines().length
    assert(after < before)
  }

}
