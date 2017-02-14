package readRdf

import org.scalatest.FunSuite
import readRdf.SplitAndFixRDFBig.{EntityLine, NewEntityLine, StatementLine, ValueNodeLine}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by espen on 10.02.17.
  */
class TestSplitAndFixRDFBig extends FunSuite{
  test("a line should be decoded correctly") {
    val newEntityLine = "<http://www.wikidata.org/entity/Q22> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.wikidata.org/ontology#Item> ."
    assert(SplitAndFixRDFBig.findLineType(newEntityLine).isInstanceOf[NewEntityLine])
    val entityLine = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P1549s> <http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6> ."
    assert(SplitAndFixRDFBig.findLineType(entityLine).isInstanceOf[EntityLine])
    assert(SplitAndFixRDFBig.findLineType(entityLine).asInstanceOf[EntityLine].property.endsWith("s>") == false)
    assert(SplitAndFixRDFBig.findLineType(entityLine).asInstanceOf[EntityLine].property.endsWith(">"))
    val statementLine = "<http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.wikidata.org/ontology#Statement> ."
    assert(SplitAndFixRDFBig.findLineType(statementLine).isInstanceOf[StatementLine])
    assert(SplitAndFixRDFBig.findLineType(statementLine).asInstanceOf[StatementLine].statementId == "<http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6>")
    val valueNodeLine = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.wikidata.org/ontology#GlobeCoordinatesValue> ."
    assert(SplitAndFixRDFBig.findLineType(valueNodeLine).isInstanceOf[ValueNodeLine])
    assert(SplitAndFixRDFBig.findLineType(valueNodeLine).asInstanceOf[ValueNodeLine].valueId == "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79>")
  }

  test("literal map should be able to decode time, quantity and geo properties") {
    val timeStatement = "<http://www.wikidata.org/entity/VTf0cf68bd9cbaa59463672583029da254> <http://www.wikidata.org/ontology#time> \"1999-01-01\"^^<http://www.w3.org/2001/XMLSchema#date> ."
    val numericStatement = "<http://www.wikidata.org/entity/VQ7f8132f69bc7b70849eb487cc23e1f0c> <http://www.wikidata.org/ontology#numericValue> \"16\"^^<http://www.w3.org/2001/XMLSchema#decimal> ."
    val geoStatementLa = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.wikidata.org/ontology#latitude> \"57.0\"^^<http://www.w3.org/2001/XMLSchema#double> ."
    val geoStatementLo = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.wikidata.org/ontology#longitude> \"-5.0\"^^<http://www.w3.org/2001/XMLSchema#double> ."
    val listOfStatements = List(timeStatement, numericStatement, geoStatementLa, geoStatementLo)
    listOfStatements.foreach(SplitAndFixRDFBig.decodeLine(_))
    assert(SplitAndFixRDFBig.valueNodeToValuesMap.size == 3)
    val lMap = SplitAndFixRDFBig.getLiteralMap
    assert(lMap.size == 4)
    println(lMap)
  }
  test("decoding on test file") {
    val source = Source.fromFile("/media/espen/Windows8_OS/SimilarEntitiesWikidata/input/smallFiles/wikidata-statements1Long.nt", enc = "utf-8").getLines()

    for(f <- source) {
      SplitAndFixRDFBig.decodeLine(f)
    }

  }
}
