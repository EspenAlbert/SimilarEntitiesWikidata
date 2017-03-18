package main

import java.io.PrintWriter

import core.query.specific.UpdateQueryFactory
import core.rdf.GraphRDF
import iAndO.dataset.ArtistDatasetReader
import org.scalatest.FunSuite
import core.strategies._
import similarityFinder.SimilarityFinder

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimilarityFinder2 extends FunSuite{
  test("similarities for obama") {
    SimilarityFinder.findTopKSimilarTo("w:Q76", 10)
  }
  test("Similars to Elton John") {
    val expectedSimilars = List("http://www.wikidata.org/entity/Q194333", "http://www.wikidata.org/entity/Q182655", "http://www.wikidata.org/entity/Q2599", "http://www.wikidata.org/entity/Q2632", "http://www.wikidata.org/entity/Q1225", "http://www.wikidata.org/entity/Q218255", "http://www.wikidata.org/entity/Q144622", "http://www.wikidata.org/entity/Q714", "http://www.wikidata.org/entity/Q272203", "http://www.wikidata.org/entity/Q1203")
    val eltonJohon = "http://www.wikidata.org/entity/Q2808"
    doTestAndStoreResultForArtist(eltonJohon, expectedSimilars)

  }
  test("Similar artist dataset, store similar entity ranking among top 1k, sim score") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile()
    var i = 0
    for((artist, similars) <- artistDataset) {
      doTestAndStoreResultForArtist(artist, similars)
    }
  }

  private def doTestAndStoreResultForArtist(artist: String, similars: List[String]) = {
    val (entityGraph: GraphRDF, strategies: Array[Strategy]) = SimilarityFinder.findGraphAndStrategiesForEntity(artist)
    val foundSimilars = SimilarityFinder.findSimilarToEntityWithStrategies(0, entityGraph, strategies)
    val entityType = entityGraph.getTypes
    val forcedSimScores = SimilarityFinder.calculateAndRankSimilarity(10, entityGraph, strategies, similars.toSet)

    def getSimScore(similar: String): Double = {
      forcedSimScores(forcedSimScores.indexWhere(_.name == similar)).score
    }

    UpdateQueryFactory.addStatementCount(artist, entityGraph.statements.size - 1) //UPDATE DB
    for (similar <- similars) {
      val similarGraph = new GraphRDF(similar)
      val similarType = similarGraph.getTypes
      val statementCount = similarGraph.statements.size - 1 //Not interested in P31
      UpdateQueryFactory.addStatementCount(similar, statementCount) //UPDATE DB
      if (entityType == similarType) {//TODO: Update to check all types
        val rankingIndex = foundSimilars.indexWhere(fs => fs.name == similar)
        if (rankingIndex > -1) {
          println("ranking = ", rankingIndex)
          val simScore = foundSimilars(rankingIndex).score
          println("sim score: ", simScore)
          UpdateQueryFactory.addResult(artist, similar, rankingIndex, simScore) //UPDATE DB
        }
        else {
          println("unable to find entity")
          val simScore = getSimScore(similar)
          println("sim score: ", simScore)
          UpdateQueryFactory.addResult(artist, similar, rankingIndex, simScore) //UPDATE DB
        }
      }
    }
    println(s"finished artists ${artist}")
  }
}
