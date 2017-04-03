package similarityFinder

import core.globals.KnowledgeGraph
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
  def setupRun(knowledgeGraph: KnowledgeGraph, runName: String, strategies : String*): Unit = {
    StrategyFactory.setupStrategyFactory(strategies)(knowledgeGraph)
    UpdateQueryFactory.addNewRun(runName)
  }
  test("Baseline search directed/undirected L=1,2 on dataset") {
    val strategies = List(
      SearchDirectedL1Strategy.name,
      SearchDirectedL2Strategy.name,
      SearchUndirectedL1Strategy.name,
      SearchUndirectedL2Strategy.name
    )
    val knowledgeGraphs = List(
      KnowledgeGraph.wikidata,
      KnowledgeGraph.dbPedia
    )
    val dataset = ArtistDatasetReader.getDatasetFromFile()
    val dbPediaDataset = ArtistDatasetReader.getDatasetDBpediaFromFile()
    val datasetSize: Int = dataset.keys.size
    val runResults = for {
      strategy <- strategies
      kg <- knowledgeGraphs
      ds = if(kg == KnowledgeGraph.dbPedia) dbPediaDataset else dataset
      runName = RunName.getRunName(List(strategy))(kg)
      a = setupRun(kg, runName, strategy)
      ((qEntity, similars), i) <- ds.zipWithIndex
      startTime = System.currentTimeMillis()
      res = new SimilarityFinder2(qEntity)(kg).findInitialEntitiesAsSet()
      execTime = System.currentTimeMillis() - startTime
      recalled = similars.filter(res.contains)
      notRecalled = similars.filterNot(recalled.contains(_))
      foundEntitiesCount = res.size
      uploaded = UpdateQueryFactory.addFindSimilarResult(runName, qEntity, recalled, notRecalled, execTime.toInt, foundEntitiesCount)
//      status = println(s"Finished $i of $datasetSize for $runName")
//    }yield (runName, qEntity, recalled, notRecalled, execTime, foundEntitiesCount)
    }yield 1
//    runResults.foreach(r => UpdateQueryFactory.addFindSimilarResult(r._1, r._2, r._3, r._4, r._5.toInt, r._6))
    ResultHandler.calculateRecall

  }

}
