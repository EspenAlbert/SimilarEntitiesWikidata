package strategy

import core.globals.KnowledgeGraph
import core.query.specific.QueryFactory
import core.strategies.{DirectLinkStrategy, PropertyMatchStrategy, StrategyFactory, ValueMatchStrategy}
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.{ActiveSlowTag, ActiveTag, TestOnlyTag}

/**
  * Created by espen on 22.03.17.
  */
class TestStrategyFactory extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val ringoStarr = WikidataFactory.ringoStarr
  test("All strategies in Wikidata should be found and created a correct strategies for", ActiveTag) {
    StrategyFactory.forceRead = true
    val sFactory = new StrategyFactory()
    val b = sFactory.mapPropertyToStrategies
    val c = sFactory.mapPropertyToStrategies
    assert(b == c)
    val directLinkStrategy = b.getOrElse(WikidataFactory.directLinkProperty, throw new Exception("fail"))
    assert(directLinkStrategy.length > 1)
    val valueMatchObjectStrategy = b.getOrElse(WikidataFactory.valueMatchObject, throw new Exception("fail"))
    assert(valueMatchObjectStrategy.length > 1)
    val propertyMatchStrategy = b.getOrElse(WikidataFactory.itemProperty, throw new Exception("fail"))
    assert(propertyMatchStrategy.length == 1)
  }
  test("Find all domain counts", ActiveSlowTag) {
    StrategyFactory.forceRead = true
    val factory = new StrategyFactory()
    val domainCountGender = WikidataFactory.domainCountGender
    val actualDomainCountGender = factory.mapPropertyToDomainCounts.getOrElse(ringoStarr.genderProp, throw new Exception("Couldn't find a value for the gender prop"))
    assert(actualDomainCountGender == domainCountGender)
  }
  test("Find domain count by reading file", ActiveTag) {
    val factory = new StrategyFactory()
    val domainCountGender = WikidataFactory.domainCountGender
    val actualDomainCountGender = factory.mapPropertyToDomainCounts.getOrElse(ringoStarr.genderProp, throw new Exception("Couldn't find a value for the gender prop"))
    assert(actualDomainCountGender == domainCountGender)
  }
  test("Find all range counts", ActiveSlowTag) {
    StrategyFactory.forceRead = true
    val factory = new StrategyFactory()
    val rangeCountOccupation = WikidataFactory.rangeCountOccupation
    val actualRangeCountOccupation = factory.mapPropertyToRangeCounts.getOrElse(ringoStarr.occupationProp, throw new Exception("Couldn't find a value for the occupation prop"))
    assert(actualRangeCountOccupation == rangeCountOccupation)
  }
  test("Find range count by reading file", ActiveTag) {
    val factory = new StrategyFactory()
    val rangeCountOccupation = WikidataFactory.rangeCountOccupation
    val actualRangeCountOccupation = factory.mapPropertyToRangeCounts.getOrElse(ringoStarr.occupationProp, throw new Exception("Couldn't find a value for the occupation prop"))
    assert(actualRangeCountOccupation == rangeCountOccupation)
  }
  test("Check time difference, reading from db vs. file") {

    QueryFactory.find100SamplesForProperty(WikidataFactory.ringoStarr.occupationProp)//TO avoid the initial delay of setting up query factory
    val sTime = System.nanoTime()
    StrategyFactory.forceRead = true
    val sFactory = new StrategyFactory()
    val endTimeDb = System.nanoTime()

    StrategyFactory.forceRead = false
    val sTimeNoDB = System.nanoTime()
    val sFactoryNoDB = new StrategyFactory()
    val endTimeNoDb = System.nanoTime()
    val readingFromDB = endTimeDb - sTime
    val readingFromFile = endTimeNoDb - sTimeNoDB
    println(s"Reading from db: $readingFromDB\n Reading from file: $readingFromFile")
    assert(readingFromDB > readingFromFile)
  }
  test("Direct link strategy for ringo starr", ActiveTag) {
    println(ringoStarr.rdfTypes)
    val propertyMatchStrategiesActual = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.spouseProp, true, ringoStarr.spouseValues)
    println(propertyMatchStrategiesActual)
    assert(propertyMatchStrategiesActual.exists(_.isInstanceOf[DirectLinkStrategy]))
  }
  test("Value match object for member of and subject for performer property", ActiveTag) {
    //Performer test (song->performer->(ringoStarr and Nils Lofgren)
    val strategiesBeforeInitializationPerformer = new StrategyFactory().mapPropertyToStrategies.getOrElse(ringoStarr.performerProp, throw new Exception("Property not found"))
    println(strategiesBeforeInitializationPerformer)
    assert(strategiesBeforeInitializationPerformer.length > 1)
    val propertyMatchStrategiesActualPerformer = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.performerProp, false, ringoStarr.performerSubject1::Nil)
    println(propertyMatchStrategiesActualPerformer)
    assert(propertyMatchStrategiesActualPerformer.exists(_.isInstanceOf[ValueMatchStrategy]))

    //Member of ((ringo starr, paul mcCartney)->memberOf->beatles)
    val strategiesBeforeInitialization = new StrategyFactory().mapPropertyToStrategies.getOrElse(ringoStarr.memberOfProp, throw new Exception("Property not found"))
    println(strategiesBeforeInitialization)
    assert(strategiesBeforeInitialization.length > 1)
    val propertyMatchStrategiesActual = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.memberOfProp, true, ringoStarr.memberOfValue::Nil)
    println(propertyMatchStrategiesActual)
    assert(propertyMatchStrategiesActual.exists(_.isInstanceOf[ValueMatchStrategy]))
  }
  test("Value match creates one strategy for each value in the range", ActiveTag) {
    val strategiesActual = StrategyFactory.getStrategies(ringoStarr.id, ringoStarr.rdfTypes, ringoStarr.occupationProp, true, ringoStarr.occupationValues)
    assert(strategiesActual.length > ringoStarr.occupationValues.length)
  }
  test("Should be able to create a count and store it in the database", ActiveTag) {
    val expectedCount = WikidataFactory.countMaleGender
    val actualCount = StrategyFactory.valueIsAPotentialValueMatchFindCount(ringoStarr.genderValue, ringoStarr.genderProp, true).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(expectedCount == actualCount)
    val cachedCount = StrategyFactory.valueIsAPotentialValueMatchFindCount(ringoStarr.genderValue, ringoStarr.genderProp, true).getOrElse(throw new Exception("Unable to find count from own database"))
    assert(cachedCount == actualCount)
  }

}
