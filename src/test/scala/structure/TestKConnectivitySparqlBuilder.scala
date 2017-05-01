package structure

import data.WikidataFactory
import org.scalatest.FunSuite
import structureFinder.KConnectivitySparqlBuilder
import structureFinder.KConnectivitySparqlBuilder.findTopStructureBetweenEntities

/**
  * Created by espen on 28.04.17.
  */
class TestKConnectivitySparqlBuilder extends FunSuite{
  test("Finding a path between Obama, Ringo Starr and Human should work") {
    val differentPathLengths = Range(1,4).foreach(length => {
      KConnectivitySparqlBuilder.pathMaxLength = length
      val wd = WikidataFactory
      val entities = List(wd.obama, wd.ringoStarr.id, wd.human)
      val topPaths = findTopStructureBetweenEntities(entities)
      assert(entities.forall(e => topPaths.exists(p => p.startEntity == e || p.endEntity == e)), s"Failed for length $length paths found: \n $topPaths")
    })

  }

}
