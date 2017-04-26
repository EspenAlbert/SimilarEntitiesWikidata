package similarityFinder

import core.globals.KnowledgeGraph
import core.rdf.{GraphRDF, GraphRDFDescriptivePropertyChecker}
import iAndO.dataset.ArtistDatasetReader
import org.scalatest.FunSuite

/**
  * Created by espen on 24.04.17.
  */
class TestTypeFiltering extends FunSuite{


  test("check fall in precision level") {
    val ds = ArtistDatasetReader.getDatasetFromFile()
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    MyConfiguration.filterOnRdfType = true
    var recalled = 0
    var notRecalled = 0
    var totalEntities = 0
    for{
      (artist, expectedSimilars) <- ds
    }{
      val qEntityTypes = new GraphRDFDescriptivePropertyChecker(artist).getTypes
      val filteredAway = expectedSimilars.filter(new GraphRDF(_).getTypes.forall(t => !qEntityTypes.contains(t)))
      val recalledI = expectedSimilars.size - filteredAway.size
      recalled += recalledI
      notRecalled += filteredAway.size
      totalEntities += expectedSimilars.size
      println(s"Recall for $artist = ${recalledI.toDouble/expectedSimilars.size}")
    }
    println(s"Total recall = ${recalled.toDouble/totalEntities}")

  }
  test("print all possible types") {
    val ds = ArtistDatasetReader.getDatasetFromFile()
    implicit val knowledgeGraph = KnowledgeGraph.wikidata
    MyConfiguration.filterOnRdfType = true
    val distinctTypes = {
      for {
        (artist, _) <- ds
        graph = new GraphRDF(artist)
        types = graph.getTypes
      } yield types
    }.flatten.toSet
    println(distinctTypes)
  }

}
