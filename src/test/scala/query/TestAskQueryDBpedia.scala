package query

import core.globals.KnowledgeGraph
import core.query.specific.AskQuery._
import data.{DBpediaFactory, WikidataFactory}
import org.scalatest.FunSuite
import tags.ActiveTag

/**
  * Created by Espen on 02.11.2016.
  */
class TestAskQueryDBpedia extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.dbPedia
  private val ringoStarr = DBpediaFactory.ringoStarr
  test("same type possible should work", ActiveTag) {
    assert(sameTypePossibleForProp(DBpediaFactory.dateProperty) == false)
    assert(sameTypePossibleForProp(ringoStarr.spouseProp) == true)
  }
  test("sharable domain should work", ActiveTag) {
    assert(sharableDomain(ringoStarr.spouseProp) == true)
    assert(sharableDomain(DBpediaFactory.quantityProperty) == false)
  }
  test("sharable range should work", ActiveTag) {
    assert(sharableRange(ringoStarr.spouseProp) == true)
    assert(sharableRange(DBpediaFactory.dateProperty) == false)
  }
  test("subject has type", ActiveTag) {
    assert(subjectHasType(ringoStarr.id, ringoStarr.rdfTypeValues))
    assert(!subjectHasType(DBpediaFactory.wales.dbpediaId, ringoStarr.rdfTypeValues))
  }


}
