package similarityFinder

import core.interlinking.Interlink
import core.query.specific.QueryFactory
import data.{DBpediaFactory, WikidataFactory}
import org.scalatest.FunSuite
import similarityFinder.ranker.SimilarEntity
import tags.ActiveTag
import similarityFinder.MultipleDatasetsSimFinder._

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by espen on 29.03.17.
  */
class TestMultipleDatasetsSimFinder extends FunSuite{
  test("Combined score for Ringo Starr") {
    findSimilarsToWikidataId(WikidataFactory.ringoStarr.id)
  }
  test("sorting a map of String,Int") {
    val m = Map[String, Double]("a" -> 1, "b" -> 0.3, "c" -> 0.6)
    val sorted = m.toList.sortBy[Double](_._2)
    println(sorted.takeRight(2))
    val mWithDefault = m.withDefaultValue(0.0)
    assert(mWithDefault("d") == 0.0)
    val mWithDefaultMapped = mWithDefault.map(identity).withDefaultValue(0.0)
    assert(mWithDefaultMapped("d") == 0.0)
  }
  test("combined score map", ActiveTag) {
    val dbp = DBpediaFactory
    val wd = WikidataFactory
    val wales = dbp.wales.wikidataId
    val beatlesDBp = dbp.ringoStarr.formerBandMemberSubject
    val wikidataSimilars: List[SimilarEntity] = List(wales, wd.obama, wd.ringoStarr.id).map(new SimilarEntity(_, Nil))
    val dbPediaSimilars : List[SimilarEntity] = List(dbp.wales.dbpediaId, dbp.ringoStarr.id, beatlesDBp).map(new SimilarEntity(_, Nil))
    val actual = findCombinedScoreMap(wikidataSimilars, dbPediaSimilars)
    val oneThirdDouble = 1.toDouble / 3
    val expected = Map[String, Double](wales -> 1, wd.ringoStarr.id -> 0.5, wd.obama -> oneThirdDouble, WikidataFactory.theBeatles.id -> oneThirdDouble/2)
    assert(actual == expected)
    val pruned = pruneRelativeRanking(actual, 2)
    assert(pruned(0)._1 == wales)
    assert(pruned(1)._1 ==wd.ringoStarr.id)
    assert(!pruned.exists(_._1 == wd.obama))
    assert(!pruned.exists(_._1 == wd.theBeatles.id))
  }
  test("prunedSimilarEntitiesForDataset", ActiveTag) {
    val dbp = DBpediaFactory
    val wd = WikidataFactory
    val wales = dbp.wales.wikidataId
    val beatlesDBp = dbp.ringoStarr.formerBandMemberSubject
    val wikidataSimilars: List[SimilarEntity] = List(wales, wd.obama, wd.ringoStarr.id).map(new SimilarEntity(_, Nil))
    val dbPediaSimilars : List[SimilarEntity] = List(dbp.wales.dbpediaId, dbp.ringoStarr.id, beatlesDBp).map(new SimilarEntity(_, Nil))
    val selected = findCombinedScoreMap(wikidataSimilars, dbPediaSimilars).map(_._1).toSeq
    val actual = prunedSimilarEntitiesForDataset(selected, wikidataSimilars)
    val expected = wikidataSimilars ++ List(new SimilarEntity(wd.theBeatles.id, Nil))
    actual.foreach(similarActual => assert(expected.exists(_.name == similarActual.name)))


    val actualDBp = prunedSimilarEntitiesForDataset(selected.map(Interlink.fromWikidataToDBpedia(_)), dbPediaSimilars)
    val expectedDBp = dbPediaSimilars ++ List(new SimilarEntity(dbp.obama, Nil))
    actualDBp.foreach(similarActual => assert(expectedDBp.exists(similarExpected=>similarExpected.name == similarActual.name), s"couldn't find ${similarActual.name}"))
  }
  test("getPrunedSimlarsBothDatasets", ActiveTag) {
    val dbp = DBpediaFactory
    val wd = WikidataFactory
    val wales = dbp.wales.wikidataId
    val beatlesDBp = dbp.ringoStarr.formerBandMemberSubject
    val wdListOfSimilars = List(wales, wd.obama, wd.ringoStarr.id).map(new SimilarEntity(_, Nil))
    val dbpListOfSimlars = List(dbp.wales.dbpediaId, dbp.ringoStarr.id, beatlesDBp).map(new SimilarEntity(_, Nil))
    val wikidataSimilars = Future{wdListOfSimilars}
    val dbPediaSimilars = Future{dbpListOfSimlars}
    val expected = wdListOfSimilars ++ List(new SimilarEntity(wd.theBeatles.id, Nil))
    val expectedDBp = dbpListOfSimlars ++ List(new SimilarEntity(dbp.obama, Nil))

    val actual = getPrunedSimlarsBothDatasets(wikidataSimilars, dbPediaSimilars)
    actual._1.foreach(similarActual => assert(expected.exists(_.name == similarActual.name)))
    actual._2.foreach(similarActual => assert(expectedDBp.exists(_.name == similarActual.name)))
  }

}
