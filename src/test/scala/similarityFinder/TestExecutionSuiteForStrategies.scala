package similarityFinder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import core.globals.{KnowledgeGraph, SimilarPropertyOntology}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.Query
import core.query.specific.UpdateQueryFactory
import core.strategies._
import data.WikidataFactory
import iAndO.dataset.ArtistDatasetReader
import org.scalatest.FunSuite
import similarityFinder.displayer.{QueryFactorySimilarityResult, ResultHandler}

/**
  * Created by espen on 31.03.17.
  */
class TestExecutionSuiteForStrategies extends FunSuite{
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer(ActorMaterializerSettings(system).withInputBuffer(2048, 2048))

  def setupRun(knowledgeGraph: KnowledgeGraph, runName: String, createRun : Boolean, strategies : String*): Unit = {
    StrategyFactory.setupStrategyFactory(strategies)(knowledgeGraph)
    if(createRun) UpdateQueryFactory.addNewRun(runName)
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
//      SearchUndirectedL1Strategy.name
      SearchUndirectedL2Strategy.name,
      SearchDirectedL2Strategy.name
//      SearchDirectedL1Strategy.name
//        DirectLinkStrategy.name
    )
    val knowledgeGraphs = List(
      KnowledgeGraph.wikidata,
      KnowledgeGraph.dbPedia
    )
    executeStrategiesOnDatasets(strategies, knowledgeGraphs, false, createNewRun = false)
//    ResultHandler.calculateRecall
//
  }
  test("Aggregator strategy") {
    val strategies = List(AggregatorStrategy.name)
    val thresholdCounts = List(500, 1000, 3000, 10000)
    for(s<-strategies;c<-thresholdCounts) {
      MyConfiguration.thresholdCountCheapStrategy = c
      executeStrategiesOnDatasets(List(s), List(KnowledgeGraph.wikidata), true)
    }
  }
  test("Expand Node Strategy") {
    val strategies = List(ExpandNodeStrategy.name)
    val thresholdCounts = List(10000)
//    val thresholdCounts = List(15000, 20000)
    for(s<-strategies;c<-thresholdCounts) {
      MyConfiguration.thresholdCountCheapStrategy = c
//      MyConfiguration.filterOnRdfType = true
//      executeStrategiesOnDatasets(List(s), List(KnowledgeGraph.wikidata), true)
//      MyConfiguration.useMustHaveProperty = true
      executeStrategiesOnDatasets(List(s), List(KnowledgeGraph.wikidata), reducedSize = true)
//      MyConfiguration.filterOnRdfType = false
//      executeStrategiesOnDatasets(List(s), List(KnowledgeGraph.wikidata), true)
    }
  }
  test("Strategy on single entity (ringo starr)") {
    val strategy = SearchUndirectedL2Strategy.name
    MyConfiguration.thresholdCountCheapStrategy = 10000
    val ringoStarr = WikidataFactory.ringoStarr.id
    val ds = ArtistDatasetReader.getDatasetFromFile().filter(_._1 == ringoStarr)
    val runName = s"http://www.espenalbert.com/rdf/resultsSimilarArtists#wikidata-${strategy}-SingleRun-RingoStarr"
    val kg = KnowledgeGraph.wikidata
    setupRun(kg, runName,createRun=true, strategy)
    executeRunOnDatasetStoreFeatureMaps(runName, ds, 1, kg)
  }

  private def executeStrategiesOnDatasets(strategies: List[String], knowledgeGraphs: List[KnowledgeGraph]= List(KnowledgeGraph.wikidata, KnowledgeGraph.dbPedia), reducedSize : Boolean = true, createNewRun : Boolean = true) = {
    val dataset = if(reducedSize) ArtistDatasetReader.getDatasetSampleWikidata() else ArtistDatasetReader.getDatasetFromFile()
    val dbPediaDataset = if(reducedSize) ArtistDatasetReader.getDatasetSampleDBpedia() else ArtistDatasetReader.getDatasetDBpediaFromFile()
    val datasetSize: Int = dataset.keys.size
    for {
      strategy <- strategies
      kg <- knowledgeGraphs
      ds = if (kg == KnowledgeGraph.dbPedia) dbPediaDataset else dataset
      runName = if(reducedSize) RunName.getRunName(List(strategy))(kg) + "-SampleRun" else RunName.getRunName(List(strategy))(kg)
    } {
      setupRun(kg, runName, createRun = createNewRun, strategy)
      println(s"starting run: $runName")
      executeRunOnDatasetStoreFeatureMaps(runName, ds, datasetSize, kg)
    }
  }
  def executeRunOnDatasetStoreFeatureMaps(runName: String, dataset: Map[String, List[String]], datasetSize : Int, knowledgeGraph: KnowledgeGraph): Unit = {
    for(((qEntity, similars), i) <- dataset.zipWithIndex;if !QueryFactorySimilarityResult.findQEntitiesForRun(runName).contains(qEntity)) {
      Query.hadTimeout = false
      val startTime = System.currentTimeMillis()
      val res = new SimilarityFinder2(qEntity, systemParam = system, materializerParam = materializer)(knowledgeGraph).findInitialEntitiesAsMap()
      val execTime = System.currentTimeMillis() - startTime
      val recalled = similars.filter(res.contains)
      .map(key => key -> res(key).toList).toMap
      val notRecalled = similars.filterNot(recalled.contains(_))
      val foundEntitiesCount = res.keys.size
      val hadTimeout = Query.hadTimeout
      UpdateQueryFactory.addFindSimilarResultWithFeatures(runName, qEntity, recalled, notRecalled, execTime.toInt, foundEntitiesCount, hadTimeout)
      println(s"Finished $i of $datasetSize for $runName")
    }
  }

  def executeRunOnDataset(runName: String, dataset: Map[String, List[String]], datasetSize : Int, knowledgeGraph: KnowledgeGraph): Unit = {
    for(((qEntity, similars), i) <- dataset.zipWithIndex;if !QueryFactorySimilarityResult.findQEntitiesForRun(runName).contains(qEntity)) {
      Query.hadTimeout = false
      val startTime = System.currentTimeMillis()
      val res = new SimilarityFinder2(qEntity, systemParam = system, materializerParam = materializer)(knowledgeGraph).findInitialEntitiesAsSet()
      val execTime = System.currentTimeMillis() - startTime
      val recalled = similars.filter(res.contains)
      val notRecalled = similars.filterNot(recalled.contains(_))
      val foundEntitiesCount = res.size
      val hadTimeout = Query.hadTimeout
      UpdateQueryFactory.addFindSimilarResult(runName, qEntity, recalled, notRecalled, execTime.toInt, foundEntitiesCount, hadTimeout)
      println(s"Finished $i of $datasetSize for $runName")
    }
  }

}
