package query

import core.feature.Feature
import core.globals.{FeatureType, KnowledgeGraph, MyDatasets}
import core.query.specific.{AskQuery, QueryFactory}
import org.scalatest.FunSuite
import tags.ActiveTag
import core.query.specific.UpdateQueryFactory._
import data.WikidataFactory
import similarityFinder.displayer.QueryFactorySimilarityResult
/**
  * Created by espen on 20.03.17.
  */
class TestUpdateQueryFactory extends FunSuite{
  test("Insert check and delete check", ActiveTag) {
    val statement = "<http://dbpedia.org/resource/iAmCustomMade> <http://dbpedia.org/resource/PropertyiAmCustomProperty> <http://dbpedia.org/resource/PropertyiAmCustomPropertyValue> ."
    val dsDBpedia = MyDatasets.DBpediaDS
    addStatements(List(statement), dsDBpedia)
    implicit val knowledgeGraph = KnowledgeGraph.dbPedia
    assert(AskQuery.ask(() => statement))
    cleanDatasetWhere(dsDBpedia, statement)
    assert(AskQuery.ask(() => statement) == false)
  }
  test("clean dataset") {
    cleanDataset(MyDatasets.dsWikidata)
  }
  test("addIsDescriptive") {
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    val genderProp = WikidataFactory.ringoStarr.genderProp
    addIsDescriptive(genderProp, true)
    val (props, isDescriptive) = QueryFactory.findIsDescriptive
    val index = props.indexWhere(_ == genderProp)
    assert(isDescriptive(index))
  }
  test("adding a result with a feature should work", ActiveTag) {
    val wd = WikidataFactory
    val runName = "http://www.espenalbert.com/rdf/resultsSimilarArtists#wikidata-ExpandNodeStrategy-SingleRun-RingoStarr"
    cleanRunName(runName)
    val f1 = new Feature(wd.ringoStarr.memberOfProp, FeatureType.searchExpandNode, 1, 1)
    val f2 = new Feature(wd.ringoStarr.occupationProp, FeatureType.searchExpandNode, 1, 2)
    val recalledMap = Map(wd.johnLennon-> List(f1,f2),
    wd.paulMcCartney -> List(f1))
    val qEntity = wd.ringoStarr.id
    addFindSimilarResultWithFeatures(runName, qEntity,recalledMap, wd.theBeatles.members.tail.tail, 5000, 10, true)
    val recalledEntities = QueryFactorySimilarityResult.findRecalledEntities(runName, qEntity)
    assert(recalledEntities == List(wd.johnLennon, wd.paulMcCartney))
    val featuresForJohnLennon = QueryFactorySimilarityResult.findFeatures(runName, qEntity, wd.johnLennon)
    assert(featuresForJohnLennon._1.contains(f1.toString))
    assert(featuresForJohnLennon._1.contains(f2.toString))
    val featuresForPaulMcCartney = QueryFactorySimilarityResult.findFeatures(runName, qEntity, wd.paulMcCartney)
    assert(featuresForPaulMcCartney._1.contains(f1.toString))
  }

}
