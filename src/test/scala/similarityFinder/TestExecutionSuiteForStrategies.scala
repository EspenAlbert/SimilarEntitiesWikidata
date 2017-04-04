package similarityFinder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import core.globals.{KnowledgeGraph, SimilarPropertyOntology}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.UpdateQueryFactory
import core.strategies._
import iAndO.dataset.ArtistDatasetReader
import org.scalatest.FunSuite
import similarityFinder.displayer.ResultHandler

/**
  * Created by espen on 31.03.17.
  */
class TestExecutionSuiteForStrategies extends FunSuite{
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system).withInputBuffer(2048, 2048))

  def setupRun(knowledgeGraph: KnowledgeGraph, runName: String, strategies : String*): Unit = {
    StrategyFactory.setupStrategyFactory(strategies)(knowledgeGraph)
    UpdateQueryFactory.addNewRun(runName)
  }
  test("Already existing strategies with config") {
    val strategies = List(
//      DirectLinkStrategy.name,
      PropertyMatchStrategy.name,
      ValueMatchStrategy.name
    )
    val thresholdCounts = List(1000, 3000, 10000)
    val useRdfType = List(true, false)
    for(s<-strategies;c<-thresholdCounts; t<-useRdfType) {
      MyConfiguration.useRdfType = t
      MyConfiguration.thresholdCountCheapStrategy = c
      executeStrategiesOnDatasets(List(s), List(KnowledgeGraph.wikidata), false)
    }

  }
  test("Baseline search directed/undirected L=1,2 on dataset") {
    val strategies = List(
//      SearchDirectedL1Strategy.name
//      SearchDirectedL2Strategy.name
//      SearchUndirectedL1Strategy.name
      SearchUndirectedL2Strategy.name
//        DirectLinkStrategy.name
    )
    val knowledgeGraphs = List(
      KnowledgeGraph.wikidata
//      KnowledgeGraph.dbPedia
    )
    executeStrategiesOnDatasets(strategies, knowledgeGraphs)
//    ResultHandler.calculateRecall
//
  }

  private def executeStrategiesOnDatasets(strategies: List[String], knowledgeGraphs: List[KnowledgeGraph]= List(KnowledgeGraph.wikidata, KnowledgeGraph.dbPedia), reducedSize : Boolean = true) = {
    val dataset = if(reducedSize) ArtistDatasetReader.getDatasetSampleWikidata() else ArtistDatasetReader.getDatasetFromFile()
    val dbPediaDataset = if(reducedSize) ArtistDatasetReader.getDatasetSampleDBpedia() else ArtistDatasetReader.getDatasetDBpediaFromFile()
    val datasetSize: Int = dataset.keys.size
    for {
      strategy <- strategies
      kg <- knowledgeGraphs
      ds = if (kg == KnowledgeGraph.dbPedia) dbPediaDataset else dataset
      runName = if(reducedSize) RunName.getRunName(List(strategy))(kg) + "-SampleRun" else RunName.getRunName(List(strategy))(kg)
    } {
      setupRun(kg, runName, strategy)
      println(s"starting run: $runName")
      executeRunOnDataset(runName, ds, datasetSize, kg)
    }
  }

  def executeRunOnDataset(runName: String, dataset: Map[String, List[String]], datasetSize : Int, knowledgeGraph: KnowledgeGraph): Unit = {
    val hadTimeoutEntity = KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph) + SimilarPropertyOntology.timeoutElement
    for(((qEntity, similars), i) <- dataset.zipWithIndex) {
      val startTime = System.currentTimeMillis()
      val res = new SimilarityFinder2(qEntity, systemParam = system, materializerParam = materializer)(knowledgeGraph).findInitialEntitiesAsSet()
      val execTime = System.currentTimeMillis() - startTime
      val recalled = similars.filter(res.contains)
      val notRecalled = similars.filterNot(recalled.contains(_))
      val foundEntitiesCount = res.size
      val hadTimeout = res.contains(hadTimeoutEntity)
      UpdateQueryFactory.addFindSimilarResult(runName, qEntity, recalled, notRecalled, execTime.toInt, foundEntitiesCount, hadTimeout)
      println(s"Finished $i of $datasetSize for $runName")
    }
  }

}
