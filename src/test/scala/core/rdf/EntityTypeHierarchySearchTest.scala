package core.rdf

import core.globals.KnowledgeGraphs
import core.testData.{DBpediaFactory, WikidataFactory}
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer

/**
  * Created by espen on 25.05.17.
  */
class EntityTypeHierarchySearchTest extends FunSuite {

  implicit val knowledgeGraph = KnowledgeGraphs.wikidata
  val wd = WikidataFactory
  test("cameFromIllegalPrecessor") {
    val searchWithoutConditions = new EntityTypeHierarchySearch(wd.band,1,1,false)
    //Should be false
    assert(!searchWithoutConditions.cameFromIllegalPrecessor(wd.rockBand, ListBuffer(), goingUp = false,stepsAwayFromRoot = 2))
    assert(!searchWithoutConditions.cameFromIllegalPrecessor(wd.rockBand, ListBuffer(wd.musicalEnsemble), goingUp = false,stepsAwayFromRoot = 2))
    assert(!searchWithoutConditions.cameFromIllegalPrecessor(wd.musicalEnsemble, ListBuffer(), goingUp = true,stepsAwayFromRoot = 2))
    assert(!searchWithoutConditions.cameFromIllegalPrecessor(wd.musicalEnsemble, ListBuffer(wd.rockBand), true,stepsAwayFromRoot = 2))
    //Should be true
    assert(searchWithoutConditions.cameFromIllegalPrecessor(wd.rockBand, ListBuffer(wd.band), goingUp = false,stepsAwayFromRoot = 2))
    assert(searchWithoutConditions.cameFromIllegalPrecessor(wd.musicalEnsemble, ListBuffer(wd.band), goingUp = true,stepsAwayFromRoot = 2))

  }
  test("should work for band") {
    val searchWithoutConditions = new EntityTypeHierarchySearch(wd.band, 1, 1,false)
    val similarEntityTypes = searchWithoutConditions.findAllEntityTypes()
    assert(similarEntityTypes.contains(wd.musicalEnsemble))
    assert(similarEntityTypes.contains(wd.rockBand))
    assert(similarEntityTypes.contains(wd.jazzBand))
    assert(similarEntityTypes.contains(wd.musicalDuo))
    println(s"Size with no restrictions: ${similarEntityTypes.size}")
    val requirementEntityTypesCountLowerThan = EntityTypeHierarchySearch.addEntityTypeIfCountEntityTypeLowerThan(1000)(_:String)
    val searchWithTypeCountRequirement = new EntityTypeHierarchySearch(wd.band, 1, 1,false, requirementEntityTypesCountLowerThan)
    val similarEntityTypesWithCountRestriction = searchWithTypeCountRequirement.findAllEntityTypes()
    assert(!similarEntityTypesWithCountRestriction.contains(wd.musicalEnsemble))
    assert(!similarEntityTypesWithCountRestriction.contains(wd.rockBand))
    assert(similarEntityTypesWithCountRestriction.contains(wd.jazzBand))
    assert(!similarEntityTypesWithCountRestriction.contains(wd.musicalDuo))
    println(s"Size with threshold=1k restriction: ${similarEntityTypesWithCountRestriction.size}")

    val searchWithPropertyOverlapRequirement = new EntityTypeHierarchySearch(wd.band, 1, 1,false, EntityTypeHierarchySearch.addEntityTypeIfPropertyDistributionSimilar(wd.band, 0.2))
    val similarTypesOverlapRequirement = searchWithPropertyOverlapRequirement.findAllEntityTypes()
    assert(similarTypesOverlapRequirement.contains(wd.rockBand))
    assert(similarTypesOverlapRequirement.contains(wd.musicalDuo))
    assert(similarTypesOverlapRequirement.contains(wd.musicalEnsemble))
    assert(similarTypesOverlapRequirement.contains(wd.jazzBand))
    println(s"Size with overlap > 0.2 restriction restriction: ${similarTypesOverlapRequirement.size}")

    val overlapThreshold02Requirement = EntityTypeHierarchySearch.addEntityTypeIfPropertyDistributionSimilar(wd.band, 0.2)(_:String)
    val searchWithPropertyOverlapAndCountRequirement = new EntityTypeHierarchySearch(wd.band, 1,6,false, requirementEntityTypesCountLowerThan,overlapThreshold02Requirement)
    val similarTypesOverlapAndCountRequirement = searchWithPropertyOverlapAndCountRequirement.findAllEntityTypes()
    println(s"Size with overlap > 0.2 restriction restriction and count restriction: ${similarTypesOverlapAndCountRequirement.size}")
  }
  test("result for human") {
//    val searchWithoutConditions = new HeuristicSearch(wd.human, 3,3,false)
//    val similarEntityTypes = searchWithoutConditions.findAllEntityTypes()
//    println(s"Size with no restrictions: ${similarEntityTypes.size}")

    val searchWithTypeCountRequirement = new EntityTypeHierarchySearch(wd.human, 3, 3,false, EntityTypeHierarchySearch.addEntityTypeIfCountEntityTypeLowerThan(5000))
    val similarEntityTypesWithCountRestriction = searchWithTypeCountRequirement.findAllEntityTypes()
    println(s"Size with threshold=1k restriction: ${similarEntityTypesWithCountRestriction.size}")

    val searchWithPropertyOverlapRequirement = new EntityTypeHierarchySearch(wd.human, 1, 1,false, EntityTypeHierarchySearch.addEntityTypeIfPropertyDistributionSimilar(wd.human, 0.7))
    val similarTypesOverlapRequirement = searchWithPropertyOverlapRequirement.findAllEntityTypes()
    println(s"Size with overlap > 0.2 restriction restriction: ${similarTypesOverlapRequirement.size}")

  }
  test("result musical artist dbp"){
    implicit val knowledgeGraph = KnowledgeGraphs.dbPedia
    val dbp = DBpediaFactory
    val searchFromMusicalArtist = new EntityTypeHierarchySearch(dbp.musicalArtist,1,1, false)
    val foundEntities = searchFromMusicalArtist.findAllEntityTypes()
    println(foundEntities)
    println(foundEntities.size)
  }

}
