package iAndO.readRdf

import org.scalatest.FunSuite
import iAndO.readRdf.SplitAndFixRDFBig._
import iAndO.readRdf.bigDataset.SplitAndFixRDFBigHelper._
import iAndO.readRdf.bigDataset.URIFixer

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by espen on 10.02.17.
  */
class TestSplitAndFixRDFBig extends FunSuite{
  test("a line should be decoded correctly") {
    val newEntityLine = "<http://www.wikidata.org/entity/Q22> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.wikidata.org/ontology#Item> ."
    assert(findLineType(newEntityLine).isInstanceOf[NewEntityLine])
    val entityLine = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P1549s> <http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6> ."
    assert(findLineType(entityLine).isInstanceOf[EntityLine])
    assert(findLineType(entityLine).asInstanceOf[EntityLine].property.endsWith("s>") == false)
    assert(findLineType(entityLine).asInstanceOf[EntityLine].property.endsWith(">"))
    val statementLine = "<http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.wikidata.org/ontology#Statement> ."
    assert(findLineType(statementLine).isInstanceOf[StatementLine])
    assert(findLineType(statementLine).asInstanceOf[StatementLine].statementId == "<http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6>")
    val valueNodeLine = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.wikidata.org/ontology#GlobeCoordinatesValue> ."
    assert(findLineType(valueNodeLine).isInstanceOf[ValueNodeLine])
    assert(findLineType(valueNodeLine).asInstanceOf[ValueNodeLine].valueId == "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79>")
  }

  test("literal map should be able to decode time, quantity and geo properties") {
    val timeStatement = "<http://www.wikidata.org/entity/VTf0cf68bd9cbaa59463672583029da254> <http://www.wikidata.org/ontology#time> \"1999-01-01\"^^<http://www.w3.org/2001/XMLSchema#date> ."
    val numericStatement = "<http://www.wikidata.org/entity/VQ7f8132f69bc7b70849eb487cc23e1f0c> <http://www.wikidata.org/ontology#numericValue> \"16\"^^<http://www.w3.org/2001/XMLSchema#decimal> ."
    val geoStatementLa = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.wikidata.org/ontology#latitude> \"57.0\"^^<http://www.w3.org/2001/XMLSchema#double> ."
    val geoStatementLo = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.wikidata.org/ontology#longitude> \"-5.0\"^^<http://www.w3.org/2001/XMLSchema#double> ."
    val listOfStatements = List(timeStatement, numericStatement, geoStatementLa, geoStatementLo)
    listOfStatements.foreach(decodeLine(_))
    assert(valueNodeToValuesMap.size == 3)
    val lMap = getLiteralMap
    assert(lMap.size == 4)
    println(lMap)
  }
  test("decoding on test file") {
    val source = Source.fromFile("input/smallFiles/wikidata-statements1Long.nt", enc = "utf-8").getLines()

    for(f <- source) {
      decodeLine(f)
    }
  }
  def getListOfValueNodeStatements() : List[String] = {
    //Entity, prop, SID
    val eToSIDTime = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P605s> <http://www.wikidata.org/entity/Q22SA0411A97-FBC4-4382-9000-C7935DA5D989> ."
    val eToSIDNum = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P2997s> <http://www.wikidata.org/entity/Q22SF3192AC9-61E1-423B-A5C7-0983C6A46D6E> ."
    val eToSIDGeo = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P625s> <http://www.wikidata.org/entity/Q22S327258B4-7023-4189-984B-E3A4A5E74B92> ."

    //Sid -> VNode
    val sIdToTimeStatement = "<http://www.wikidata.org/entity/Q22SA0411A97-FBC4-4382-9000-C7935DA5D989> <http://www.wikidata.org/entity/P580q> <http://www.wikidata.org/entity/VTf0cf68bd9cbaa59463672583029da254> ."
    val sIdToNumericStatement = "<http://www.wikidata.org/entity/Q22SF3192AC9-61E1-423B-A5C7-0983C6A46D6E> <http://www.wikidata.org/entity/P2997v> <http://www.wikidata.org/entity/VQ7f8132f69bc7b70849eb487cc23e1f0c> ."
    val sIdToGeoStatement = "<http://www.wikidata.org/entity/Q22S327258B4-7023-4189-984B-E3A4A5E74B92> <http://www.wikidata.org/entity/P625v> <http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> ."

    //Value statements
    val timeStatement = "<http://www.wikidata.org/entity/VTf0cf68bd9cbaa59463672583029da254> <http://www.wikidata.org/ontology#time> \"1999-01-01\"^^<http://www.w3.org/2001/XMLSchema#date> ."
    val numericStatement = "<http://www.wikidata.org/entity/VQ7f8132f69bc7b70849eb487cc23e1f0c> <http://www.wikidata.org/ontology#numericValue> \"16\"^^<http://www.w3.org/2001/XMLSchema#decimal> ."
    val geoStatementLa = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.wikidata.org/ontology#latitude> \"57.0\"^^<http://www.w3.org/2001/XMLSchema#double> ."
    val geoStatementLo = "<http://www.wikidata.org/entity/VC2e73d63b7d980f6998ee0feeb4818e79> <http://www.wikidata.org/ontology#longitude> \"-5.0\"^^<http://www.w3.org/2001/XMLSchema#double> ."

    return List(eToSIDGeo, eToSIDNum, eToSIDTime, sIdToTimeStatement, sIdToNumericStatement, sIdToGeoStatement, timeStatement, numericStatement, geoStatementLa, geoStatementLo)
  }
  test("find statements and ranks should be able extract the statements that are value nodes") {
    val lines = getListOfValueNodeStatements()
    for(l <- lines) {
      decodeLine(l)
    }
    assert(statementIdMap.size == 3)
    assert(statementIdToValuesMap.size == 3)
    assert(valueNodeToValuesMap.size == 3)

    val literalMap = getLiteralMap
    assert(literalMap.size == 4)
    val statementsAndRanks = findStatementsAndRanks(literalMap)
    assert(statementsAndRanks.length == 4)
    println(statementsAndRanks)
    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P605P580q>"))
    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P625la>"))
    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P625lo>"))
    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P2997>"))
  }
  test("find statements and ranks should be able extract the statements that are literal values") {
    //Entity, prop, SID
    val eToSIDString = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P1549s> <http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6> ."
    val eToSIDNum = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P1036s> <http://www.wikidata.org/entity/Q22S7b6061b8-4311-099d-3d2f-3a7de56cf23a> ."
    val eToSIDWikimedia = "<http://www.wikidata.org/entity/Q22> <http://www.wikidata.org/entity/P18s> <http://www.wikidata.org/entity/Q22S1573baad-472f-cd6f-66f8-702a7f7c37f7> ."

    //Sid -> VNode
    val sIdToStringValue = "<http://www.wikidata.org/entity/Q22S540b079c-43d8-e0ca-43b2-431f1a9f41f6> <http://www.wikidata.org/entity/P1549v> \"Scottish\"@en ."
    val sIdToNumericStatement = "<http://www.wikidata.org/entity/Q22S7b6061b8-4311-099d-3d2f-3a7de56cf23a> <http://www.wikidata.org/entity/P1036v> \"2--411\" ."
    val sIdToNumericStatementType = "<http://www.wikidata.org/entity/Q22S7b6061b8-4311-099d-3d2f-3a7de56cf23a> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.wikidata.org/ontology#Statement> ."
    val sIdToWikimedia = "<http://www.wikidata.org/entity/Q22S1573baad-472f-cd6f-66f8-702a7f7c37f7> <http://www.wikidata.org/entity/P18v> <http://commons.wikimedia.org/wiki/File:Highlands_(6194583897).jpg> ."

//    val newEntityLine = "<abc> <http://www.wikidata.org/ontology#Item> <def>"
    val lines = List(eToSIDString, eToSIDNum, eToSIDWikimedia, sIdToStringValue, sIdToNumericStatement,sIdToNumericStatementType, sIdToWikimedia)
    for(l <- lines) {
      decodeLine(l)
    }
    assert(statementIdMap.size == 3)
    assert(statementIdToValuesMap.size == 3)
    assert(valueNodeToValuesMap.size == 0)

    val literalMap = getLiteralMap
    assert(literalMap.size == 0)
    val statementsAndRanks = findStatementsAndRanks(literalMap)
    println(statementsAndRanks)
    addStatements()
    assert(statementsAndRanks.length == 3)
    val source = Source.fromFile("input/smallFiles/wikidata-statements1-test.nt", enc = "utf-8").getLines()
    assert(source.length == 3)
    //    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P605P580q>"))
//    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P625la>"))
//    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P625lo>"))
//    assert(statementsAndRanks.exists(_._1._2 == "<http://www.wikidata.org/entity/P2997>"))
  }
  test("Decoding a line with spaces in value should work") {
    val spaceLine = "<http://www.wikidata.org/entity/Q31S5DCB7F90-B5B0-4F27-AAE6-DD915774E62A> <http://www.wikidata.org/entity/P935v> \"Belgi\\u00EB - Belgique\" ."
    assert(findLineType(spaceLine).isInstanceOf[StatementLine])
  }
  test("Statements with literal values should always have only 1 value whenever a preferred rank is present") {
    testActive = true
    val source = Source.fromFile("input/smallFiles/testRank.nt", enc = "utf-8").getLines()

    for(f <- source) {
      decodeLine(f)
    }
    addStatements()
  }
  test("createValueNodeStatements") {
    val lines = getListOfValueNodeStatements()
    lines.foreach(decodeLine(_))
    val literalMap = getLiteralMap
    val statements = createValueNodeStatements(literalMap)
    println(statements)
    statements.map(_.split(" ")).foreach((arr) => assert(arr(0).endsWith(">")))
    uploadValueNodeStatements(statements)
  }
  //createValueNodeStatements must run first...
  //Manual check...
  test("database able to resolve missing value nodes") {
    val lines = getListOfValueNodeStatements().filterNot(_ == "<http://www.wikidata.org/entity/VTf0cf68bd9cbaa59463672583029da254> <http://www.wikidata.org/ontology#time> \"1999-01-01\"^^<http://www.w3.org/2001/XMLSchema#date> .")
    lines.foreach(decodeLine(_))
    addStatements()
  }
  test("Some possible error statements") {
    val error1 = "<http://www.wikidata.org/entity/Q26087> <http://www.wikidata.org/entity/P18> <http://commons.wikimedia.org/wiki/File:Bundesarchiv_Bild_101I-114-0069-05,_Nordeuropa,_\\\"schwimmende_Frontbuchhandlung\\\".jpg> ."
    uploadToDataset(List(error1))
    assert(URIFixer.fixFixableURIs(List(error1)).size == 0)
  }
}
