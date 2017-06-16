package query

import core.globals.{KnowledgeGraphs, MyDatasets}
import org.scalatest.FunSuite
import core.query.specific.AskQuery._
import core.testData.WikidataFactory
import org.scalameter
import tags.ActiveTag
import org.scalameter._
/**
  * Created by Espen on 02.11.2016.
  */
class TestAskQueryWikidata extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraphs.wikidata
  test("same type possible should work", ActiveTag) {
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1291") == false)
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1290") == true)
    assert(sameTypePossibleForProp("http://www.wikidata.org/entity/P1283") == false)
  }
  test("sharable domain should work", ActiveTag) {
    assert(sharableDomain("http://www.wikidata.org/entity/P175") == true)
    assert(sharableDomain("http://www.wikidata.org/entity/P1283") == false)
  }
  test("sharable range should work", ActiveTag) {
    assert(sharableRange("http://www.wikidata.org/entity/P175") == true)
    assert(sharableRange("http://www.wikidata.org/entity/P1283") == false)
  }
  test("subject has type", ActiveTag) {
    val (graphKristiansand, typesKristiansand) = WikidataFactory.kristiansandAndTypes()
    assert(subjectHasType(graphKristiansand.entity, typesKristiansand))
    assert(!subjectHasType(graphKristiansand.entity, List(WikidataFactory.human)))
  }
  test("maxCountSameSubject", ActiveTag) {
    assert(!maxCountSameSubject(WikidataFactory.ringoStarr.spouseProp))
    assert(maxCountSameSubject(WikidataFactory.ringoStarr.countryOfCitizenShipProperty))
  }
  test("oneOfTypesUsedInDomainOrRange") {

    val askTime = withWarmer(new scalameter.Warmer.Default) measure {
      assert(oneOfTypesUsedInDomainOrRange(WikidataFactory.human::WikidataFactory.jazzBand::Nil, WikidataFactory.ringoStarr.spouseProp, true, true) == (true, true))
    }
    println(askTime)
    assert(askTime.value < 100)
  }
  test("existValueMatchForProperty") {
    val wd = WikidataFactory
    assert(existsValueMatchForPropertyWithCountGreaterThan(wd.ringoStarr.countryOfCitizenShipProperty))
    assert(!existsValueMatchForPropertyWithCountGreaterThan(wd.ringoStarr.performerProp))
  }


}
