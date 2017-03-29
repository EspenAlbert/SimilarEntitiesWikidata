package similarityFinder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, ClosedShape}
import akka.stream.scaladsl.{Broadcast, Concat, Flow, GraphDSL, Keep, RunnableGraph, Sink, Source, Zip}
import core.globals.KnowledgeGraph
import core.interlinking.Interlink
import core.query.specific.QueryFactory
import similarityFinder.ranker.SimilarEntity

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


//  def foldSimilarEntities : Sink[List[SimilarEntity], Future[List[SimilarEntity]]] = Sink.fold(List[SimilarEntity]())(_ ++ _)
  def findSimilarsToWikidataId(qEntityWikidata : String): Unit = {
    val dbPediaIdFuture = Future {
      QueryFactory.findIdDBpediaFromWikidataId(qEntityWikidata)
    }
    val simFinderWikidata = new SimilarityFinder2(qEntityWikidata)(KnowledgeGraph.wikidata)
    val foundEntitiesBeforePruningWikidata = Future {
      simFinderWikidata.findInitialEntities()
    }
    val dbPediaId = Await.result(dbPediaIdFuture, 5 seconds)

    val simFinderDBpedia : SimilarityFinder2 = dbPediaId match {
      case Success(dbPediaId) => new SimilarityFinder2(dbPediaId)(KnowledgeGraph.dbPedia)
      case _ => {
        println(s"Unable to find db pedia id for: $qEntityWikidata")
        new SimilarityFinder2(qEntityWikidata)(KnowledgeGraph.dbPedia)
      }
    }
    val foundEntitiesBeforePruningDBpedia = Future{
      simFinderDBpedia.findInitialEntities()
    }
    val sourceWikidataInitials = Source.fromFuture(foundEntitiesBeforePruningWikidata)
    val sourceDBpediaInitials = Source.fromFuture(foundEntitiesBeforePruningDBpedia)
    val combined = sourceWikidataInitials.zip(sourceDBpediaInitials)
    combined.via(Flow[(List[SimilarEntity],List[SimilarEntity])])

  }


}
