package query

import core.globals.{KnowledgeGraph, MyDatasets}
import core.query.specific.AskQuery
import org.scalatest.FunSuite
import tags.ActiveTag
import core.query.specific.UpdateQueryFactory._
/**
  * Created by espen on 20.03.17.
  */
class TestUpdateQueryFactory extends FunSuite{
  test("Insert check and delete check", ActiveTag) {
    val statement = "<http://dbpedia.org/resource/iAmCustomMade> <http://dbpedia.org/resource/PropertyiAmCustomProperty> <http://dbpedia.org/resource/PropertyiAmCustomPropertyValue> ."
    val dsDBpedia = MyDatasets.DBpediaDS
    addStatements(List(statement), dsDBpedia)
    implicit val knowledgeGraph = KnowledgeGraph.dbPedia
    assert(AskQuery.ask(() => statement))
    cleanDatasetWhere(dsDBpedia, statement)
    assert(AskQuery.ask(() => statement) == false)
  }
  test("clean dataset") {
    cleanDataset(MyDatasets.dsWikidata)
  }

}
