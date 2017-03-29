package similarityFinder

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink, Source}
import akka.stream.{ActorMaterializer, ClosedShape}
import core.globals.KnowledgeGraph
import data.{DBpediaFactory, WikidataFactory}
import org.scalatest.FunSuite
import similarityFinder.displayer.Displayer

/**
  * Created by espen on 21.03.17.
  */
class TestSimilarityFinderDBpedia  extends FunSuite{
  implicit val knowledgeGraph = KnowledgeGraph.dbPedia
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  import scala.concurrent.ExecutionContext.Implicits.global

  val ringoStarr= DBpediaFactory.ringoStarr
  val ringoStarrSimFinder = new SimilarityFinder2(ringoStarr.id)


  test("It should work for ringo starr on dbpedia dataset!!") {
    val simEntities = ringoStarrSimFinder.findSimilarEntities()
    assert(simEntities.size == SimilarityFinder2.ENTITIES_AFTER_PRUNING)
    Displayer.displayResult(simEntities, 10, ringoStarr.id)
  }
}
