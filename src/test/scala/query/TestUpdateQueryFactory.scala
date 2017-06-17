package query

import core.globals.{KnowledgeGraphs, MyDatasets, SimilarPropertyOntology}
import core.query.specific.{AskQuery, QueryFactory, QueryFactoryJena}
import org.scalatest.FunSuite
import tags.ActiveTag
import core.query.specific.UpdateQueryFactory._
import core.rdf.TypePropertyDistributionFinder
import core.testData.WikidataFactory

import scala.util.{Failure, Success}
/**
  * Created by espen on 20.03.17.
  */
class TestUpdateQueryFactory extends FunSuite{
  test("Insert check and delete check", ActiveTag) {
    val statement = "<http://dbpedia.org/resource/iAmCustomMade> <http://dbpedia.org/resource/PropertyiAmCustomProperty> <http://dbpedia.org/resource/PropertyiAmCustomPropertyValue> ."
    val dsDBpedia = MyDatasets.dsDBpedia
    addStatements(List(statement), dsDBpedia)
    implicit val knowledgeGraph = KnowledgeGraphs.dbPedia
    assert(AskQuery.ask(() => statement))
    cleanDatasetWhere(dsDBpedia, statement)
    assert(AskQuery.ask(() => statement) == false)
  }
  test("clean dataset") {
    cleanDataset(MyDatasets.resultsSimilarArtists)
  }
  test("addIsDescriptive") {
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
    val genderProp = WikidataFactory.ringoStarr.genderProp
    addIsDescriptive(genderProp, true)
    val (props, isDescriptive) = QueryFactory.findIsDescriptive
    val index = props.indexWhere(_ == genderProp)
    assert(isDescriptive(index))
  }
  //TODO: Move test
//  test("should be able to find and store label for Ringo Starr") {
//    QueryFactorySimilarityResult.findLabelForEntity(WikidataFactory.ringoStarr.id) match{
//      case Success(label) => assert(label == "Ringo Starr")
//      case Failure(f) => assert(false, s"Failed to find label $f")
//    }
//  }
//  test("adding a result with a feature should work", ActiveTag) {
//    val wd = WikidataFactory
//    val runName = "http://www.espenalbert.com/rdf/resultsSimilarArtists#wikidata-ExpandNodeStrategy-SingleRun-RingoStarr"
//    addNewRun(runName)
//    val f1 = new Feature(wd.ringoStarr.memberOfProp, FeatureType.searchExpandNode, 1, 1)
//    val f2 = new Feature(wd.ringoStarr.occupationProp, FeatureType.searchExpandNode, 1, 2)
//    val recalledMap = Map(wd.johnLennon-> List(f1,f2),wd.paulMcCartney -> List(f1))
//    val qEntity = wd.ringoStarr.id
//    addFindSimilarResultWithFeatures(runName, qEntity,recalledMap, wd.theBeatles.members.tail.tail, 5000, 10, true)
//    val recalledEntities = QueryFactorySimilarityResult.findRecalledEntities(runName, qEntity)
//    assert(recalledEntities == List(wd.johnLennon, wd.paulMcCartney))
//    val featuresForJohnLennon = QueryFactorySimilarityResult.findFeatures(runName, qEntity, wd.johnLennon)
//    assert(featuresForJohnLennon._1.contains(f1.toString))
//    assert(featuresForJohnLennon._1.contains(f2.toString))
//    val featuresForPaulMcCartney = QueryFactorySimilarityResult.findFeatures(runName, qEntity, wd.paulMcCartney)
//    assert(featuresForPaulMcCartney._1.contains(f1.toString))
//    //Adding extra result
//    val recalledMap2 = Map(wd.paulMcCartney-> List(f1,f2),
//    qEntity -> List(f1))
//    addFindSimilarResultWithFeatures(runName, wd.johnLennon,recalledMap2, wd.theBeatles.members.tail.tail, 1111, 6, true)
//  }
  test("updating a previous value in the ds should work") {
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
    val firstLevel = 50
    val secondLevel = 0
    val entityType = WikidataFactory.entityType
    updateHierachyLevel(entityType, firstLevel)
    assert(QueryFactoryJena.hierachyLevel(entityType).get == firstLevel)
    updateHierachyLevel(entityType, secondLevel)
    assert(QueryFactoryJena.hierachyLevel(entityType).get == secondLevel)
  }
  test("adding property distribution for rock band should work") {
    implicit val knowledgeGraph = KnowledgeGraphs.wikidata
    val wd = WikidataFactory
    cleanDatasetWhere(MyDatasets.strategyMappingWikidata, s"<${wd.rockBand}> <${SimilarPropertyOntology.propertyDistributionNode}> ?pdn")
    val propertyDistribution = TypePropertyDistributionFinder.propertyDistributionIgnoreRareness(wd.rockBand)
    addPropertyDistribution(wd.rockBand, propertyDistribution)
    val rockBandsWithPerformerProp = QueryFactoryJena.typePropertyCountLocal(wd.rockBand, wd.ringoStarr.performerProp, false)
    assert(rockBandsWithPerformerProp > 500)
  }

}
