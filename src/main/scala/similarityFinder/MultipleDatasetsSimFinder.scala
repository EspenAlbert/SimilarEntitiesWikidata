package similarityFinder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import akka.stream.scaladsl.{Broadcast, Concat, Flow, GraphDSL, Keep, RunnableGraph, Sink, Source, Zip}
import core.globals.KnowledgeGraph
import core.interlinking.Interlink
import core.query.specific.QueryFactory
import core.rdf.GraphRDF
import core.strategies.Strategy
import similarityFinder.ranker.SimilarEntity

import scala.collection.parallel.mutable.ParIterable
import scala.concurrent.Future
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Success
/**
  * Created by espen on 29.03.17.
  */
object MultipleDatasetsSimFinder {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system).withInputBuffer(2048, 2048))
  import GraphDSL.Implicits._


  def foldSimilarEntities : Sink[List[SimilarEntity], Future[List[SimilarEntity]]] = Sink.fold(List[SimilarEntity]())(_ ++ _)
  def findSimilarsToWikidataId(qEntityWikidata : String): Unit = {
    val dbPediaIdFuture = Future {
      QueryFactory.findIdDBpediaFromWikidataId(qEntityWikidata)
    }
    val simFinderWikidata = new SimilarityFinder2(qEntityWikidata)(KnowledgeGraph.wikidata)
    val foundEntitiesBeforePruningWikidata = Future {
      simFinderWikidata.findInitialEntities()
    }
    val dbPediaId = Await.result(dbPediaIdFuture, 5 seconds)

    val simFinderDBpedia: SimilarityFinder2 = dbPediaId match {
      case Success(dbPediaId) => new SimilarityFinder2(dbPediaId)(KnowledgeGraph.dbPedia)
      case _ => {
        println(s"Unable to find db pedia id for: $qEntityWikidata")
        new SimilarityFinder2(qEntityWikidata)(KnowledgeGraph.dbPedia)
      }
    }
    val foundEntitiesBeforePruningDBpedia = Future {
      simFinderDBpedia.findInitialEntities()
    }
    val (prunedWikidata, prunedDBpedia) = getPrunedSimlarsBothDatasets(foundEntitiesBeforePruningWikidata, foundEntitiesBeforePruningDBpedia)
    val finalSimilarsWikidataFuture = Future {
      simFinderWikidata.findSimilarToRestOfStrategies(prunedWikidata)
    }
    val finalSimilarsDBpediaFuture = Future {
      simFinderDBpedia.findSimilarToRestOfStrategies(prunedDBpedia)
    }
    println("About to combine the result")
    val (finalWikidata, finalDBpedia) = getPrunedSimlarsBothDatasets(finalSimilarsWikidataFuture, finalSimilarsDBpediaFuture)
    val finalRankingWikidata = Await.result(finalSimilarsWikidataFuture, 2 seconds)
    println(s"Final ranking wikidata list of simEntities: $finalRankingWikidata")
    val finalRankingDBpedia = Await.result(finalSimilarsDBpediaFuture, 2 seconds)
    for(((seWikidata, seDBpedia),index) <- finalWikidata.zip(finalDBpedia).zipWithIndex) {
      println(s"Similar combined # $index in Wikidata: ${seWikidata.name} with ranking: ${finalRankingWikidata.indexWhere(_.name ==seWikidata.name)} in DBpedia: ${seDBpedia.name} with ranking: ${finalRankingDBpedia.indexWhere(_.name ==seDBpedia.name)}")
    }
  }

  def getPrunedSimlarsBothDatasets(foundEntitiesBeforePruningWikidata: Future[List[SimilarEntity]], foundEntitiesBeforePruningDBpedia: Future[List[SimilarEntity]]): (List[SimilarEntity], List[SimilarEntity]) = {
    val sourceWikidataInitials = Source.fromFuture(foundEntitiesBeforePruningWikidata)
    val sourceDBpediaInitials = Source.fromFuture(foundEntitiesBeforePruningDBpedia)
    val combinedSource = sourceWikidataInitials.zip(sourceDBpediaInitials)
    val runnedGraph = combinedSource.via(Flow[(List[SimilarEntity], List[SimilarEntity])]
      .map {
        case (listWD, listDB) => {
          val combinedMap: Map[String, Double] = findCombinedScoreMap(listWD, listDB)
          val pruned = pruneRelativeRanking(combinedMap)
          val prunedWD = prunedSimilarEntitiesForDataset(pruned.map(_._1), listWD)
          val prunedDBp = prunedSimilarEntitiesForDataset(pruned.map(t => Interlink.fromWikidataToDBpedia(t._1)), listDB)
          (Source(List(prunedWD)).toMat(foldSimilarEntities)(Keep.right).run(),
            Source(List(prunedDBp)).toMat(foldSimilarEntities)(Keep.right).run())
        }
      }).toMat(Sink.head)(Keep.right).run()
    val graphComplete = Await.result(runnedGraph, 10 minutes)
    return (Await.result(graphComplete._1, 10 minutes), (Await.result(graphComplete._2, 10 minutes)))
  }

  def prunedSimilarEntitiesForDataset(selectedEntities : Seq[String], withoutPruning : List[SimilarEntity]): List[SimilarEntity] = {
    val vectorWithoutPruning = withoutPruning.toVector
    return selectedEntities.map(entity => vectorWithoutPruning.indexWhere(entity == _) match {
      case -1 => new SimilarEntity(entity, Nil)
      case a if(a > 0) => vectorWithoutPruning(a)
    }).toList
  }

  def pruneRelativeRanking(combinedMap: Map[String, Double], entitiesToInclude : Int = SimilarityFinder2.ENTITIES_AFTER_PRUNING) = {
    combinedMap.toList.sortBy[Double](_._2).takeRight(entitiesToInclude).reverse
  }

  def findCombinedScoreMap(listWD: List[SimilarEntity], listDB: List[SimilarEntity]) = {
    val wdMap = convertToRelativeRanking(listWD)
    val dbMap = convertToRelativeRanking(listDB)
      .map(nameAndValue => (Interlink.fromDBpediaToWikidata(nameAndValue._1) -> nameAndValue._2))
      .withDefaultValue(0.0)

    val combinedMap = wdMap.map {
      case (entity, relativeRanking) => entity -> ((relativeRanking + dbMap(entity)) / 2)
    } ++ dbMap.filterKeys(!wdMap.contains(_)).map {
      case (entity, relativeRanking) => entity -> (relativeRanking / 2)
    }
    combinedMap
  }

  def convertToRelativeRanking(similarEntities : List[SimilarEntity]): Map[String, Double] = {
    val size = similarEntities.size
    return similarEntities
    .zipWithIndex
    .map{case (se, index) => se.name -> ((size.toDouble - index)/size)}
    .toMap
    .withDefaultValue(0.0)
  }


}
