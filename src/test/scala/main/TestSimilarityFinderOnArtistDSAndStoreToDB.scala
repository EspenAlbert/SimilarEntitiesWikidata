package main

import java.io.PrintWriter
import java.util.NoSuchElementException

import core.globals.KnowledgeGraph
import core.query.specific.UpdateQueryFactory
import core.rdf.GraphRDF
import iAndO.dataset.ArtistDatasetReader
import org.scalatest.FunSuite
import core.strategies._
import similarityFinder.SimilarityFinder2

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimilarityFinderOnArtistDSAndStoreToDB extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  test("Similars to Elton John") {
    val expectedSimilars = List("http://www.wikidata.org/entity/Q194333", "http://www.wikidata.org/entity/Q182655", "http://www.wikidata.org/entity/Q2599", "http://www.wikidata.org/entity/Q2632", "http://www.wikidata.org/entity/Q1225", "http://www.wikidata.org/entity/Q218255", "http://www.wikidata.org/entity/Q144622", "http://www.wikidata.org/entity/Q714", "http://www.wikidata.org/entity/Q272203", "http://www.wikidata.org/entity/Q1203")
    val eltonJohn = "http://www.wikidata.org/entity/Q2808"
    doTestAndStoreResultForArtist(eltonJohn, expectedSimilars)

  }
  test("Similar artist dataset, store similar entity ranking among top 1k, sim score") {
    val artistDataset = ArtistDatasetReader.getDatasetFromFile().take(3)
    var i = 0
    for((artist, similars) <- artistDataset) {
      doTestAndStoreResultForArtist(artist, similars)
    }
  }

  private def doTestAndStoreResultForArtist(artist: String, similars: List[String]) = {
    val simFinder = new SimilarityFinder2(artist)
    val foundSimilars = simFinder.findSimilarEntities()
    val graphQEntity = simFinder.qEntityGraph
    val entityTypes = graphQEntity.getTypes
    val forcedSimScores = simFinder.findSimilarityTo(similars)

    def getSimScore(similar: String): Double = {
      forcedSimScores(forcedSimScores.indexWhere(_.name == similar)).score
    }

    UpdateQueryFactory.addStatementCount(artist, graphQEntity.getStatementCountWithoutTypeStatements) //UPDATE DB
    for (similar <- similars) {
      val similarGraph = new GraphRDF(similar)
      val similarTypes = similarGraph.getTypes
      val statementCount = similarGraph.getStatementCountWithoutTypeStatements //Not interested in P31
      UpdateQueryFactory.addStatementCount(similar, statementCount) //UPDATE DB
      if (entityTypes.exists((eType) => similarTypes.contains(eType))) {
        val rankingIndex = foundSimilars.indexWhere(fs => fs.name == similar)
        if (rankingIndex > -1) {
          println(s"ranking = $rankingIndex for entity $similar")
          val simScore = foundSimilars(rankingIndex).score
          println("sim score: ", simScore)
          UpdateQueryFactory.addResult(artist, similar, rankingIndex, simScore) //UPDATE DB
        }
        else {
          println(s"unable to find entity $similar")
          val simScore = getSimScore(similar)
          println("sim score: ", simScore)
          UpdateQueryFactory.addResult(artist, similar, rankingIndex, simScore) //UPDATE DB
        }
      }
    }
    try {
    println(s"Bottom ranked artist was ${foundSimilars.last.name} and had score ${foundSimilars.last.score}")
    println(s"100th most simlar was ${foundSimilars(100).name} and had score ${foundSimilars(100).score}")
    } catch {
      case a : NoSuchElementException => println("Unable to print stats")
    }
    println(s"finished artists ${artist}")
  }
}
