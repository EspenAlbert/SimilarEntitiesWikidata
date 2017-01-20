package ownOntologyPopularizer

import org.scalatest.FunSuite

import scala.io.Source

/**
  * Created by Espen on 01.11.2016.
  */
class TestInsertPropertiesIntoOntology extends FunSuite{
  test("a file for all the properties (2398) must be created") {
    InsertPropertiesIntoOntology.insertProperties()
    val listOfStatements = Source.fromFile("output/propertyMappedToOntology.nt").getLines().toList
    assert(listOfStatements.length == 2398)
  }
}
