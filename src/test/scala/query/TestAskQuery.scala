package query

import core.globals.{KnowledgeGraph, MyDatasets}
import org.scalatest.FunSuite
import core.query.specific.AskQuery._
import data.WikidataFactory

/**
  * Created by Espen on 02.11.2016.
  */
class TestAskQuery extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("same type possible should work") {
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1291") == false)
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1290") == true)
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1283") == false)
  }
  test("sharable domain should work") {
    assert(sharableDomain("http://www.wikidata.org/entity/P175") == true)
    assert(sharableDomain("http://www.wikidata.org/entity/P1283") == false)
  }
  test("sharable range should work") {
    assert(sharableRange("http://www.wikidata.org/entity/P175") == true)
    assert(sharableRange("http://www.wikidata.org/entity/P1283") == false)
  }
  test("subject has type") {
    val (graphKristiansand, typesKristiansand) = WikidataFactory.kristiansandAndTypes()
    assert(subjectHasType(graphKristiansand.entity, typesKristiansand))
    assert(!subjectHasType(graphKristiansand.entity, List(WikidataFactory.human)))
  }


}
