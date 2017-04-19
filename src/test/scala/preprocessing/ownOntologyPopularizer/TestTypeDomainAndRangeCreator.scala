package preprocessing.ownOntologyPopularizer

import core.globals.KnowledgeGraph
import core.query.specific.QueryFactory
import data.WikidataFactory
import org.scalatest.FunSuite
import tags.ActiveTag

/**
  * Created by espen on 19.04.17.
  */
class TestTypeDomainAndRangeCreator extends FunSuite{
  test("Generation of domain and range types for the lifestyleproperty should work", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val wd = WikidataFactory
    val expectedDomainTypes = wd.domainTypesLifestyleProp
    val expectedRangeTypes = wd.rangeTypesLifestyleProp
    val lifestyleProp = wd.ringoStarr.lifestyleProp
    TypeDomainAndRangeCreator.findAndStoreDomainAndRangeTypesForProperty(lifestyleProp)
    val actualDomains = QueryFactory.findDomainTypesForPropertyLocally(lifestyleProp)
    val actualRanges = QueryFactory.findRangeTypesForPropertyLocally(lifestyleProp)
    assert(expectedDomainTypes.forall(actualDomains.contains(_)))
    assert(expectedRangeTypes.forall(actualRanges.contains(_)))
  }


}
