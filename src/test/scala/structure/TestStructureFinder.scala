package structure

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import structureFinder.StructureFinder
/**
  * Created by espen on 23.01.17.
  */
class TestStructureFinder extends FunSuite{
  val entities = List(
    "http://www.wikidata.org/entity/Q76",
    "http://www.wikidata.org/entity/Q649593",
    "http://www.wikidata.org/entity/Q15982321"
  )
  val movieEntities = List(
    "http://www.wikidata.org/entity/Q35332",
    "http://www.wikidata.org/entity/Q153723",
    "http://www.wikidata.org/entity/Q3772"
  )
  test("aEntityIsObjectFunction returns true when it should") {
    val oEntities = entities.filterNot(_ == entities(0))
    val statement = (entities(0), "property", entities(2))
    assert(StructureFinder.aEntityIsObject(oEntities, entities(2)))
    assert(!StructureFinder.aEntityIsObject(oEntities, "rolf"))
  }
  test("structure finder should work for obama case") {
    StructureFinder.findStructures(entities)
  }
  test("structure finder should work for movie case") {
    StructureFinder.findStructures(movieEntities)
  }
  test("regex play") {
    val s = "select ?0 ?1 ?2\nwhere {{\n?0<http://www.wikidata.org/entity/P22>?1 .\n?0<http://www.wikidata.org/entity/P7>?2 .\n}UNION \n { GRAPH ?g\nwhere {\n?0<http://www.wikidata.org/entity/P22>?1 .\n?0<http://www.wikidata.org/entity/P7>?2 .\n}}}"
    val splittedOnWhere = s.split("where")
    println(splittedOnWhere(0) + "where " + splittedOnWhere(1) + splittedOnWhere(2))
  }

}
