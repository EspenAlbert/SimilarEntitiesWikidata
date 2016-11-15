package rdf

import globals.PrimitiveDatatype
import org.scalatest.FunSuite
import query.variables.StaticQueryVariable

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimpleRDF extends FunSuite{
  test("A file should be created properly") {
    val statement = new SimpleRDF(new StaticQueryVariable("subject"), new StaticQueryVariable("predicate"), new StaticQueryVariable("object"))
    CreateRdfFile.createRDFFile(List(statement), "test")
    val textFromFile = Source.fromFile("output/test.nt").getLines().next()
    assert(textFromFile == statement.getStatementNt())
  }
  test("A statement with a datatype must be written correctly to file") {
    val datatypes = PrimitiveDatatype.values
    val standardValue = "I am a champion"
    val statements = new ArrayBuffer[SimpleRDF]()
    for(d <- datatypes) {
      statements.append(new SimpleRDF(new StaticQueryVariable("w:Q76"), new StaticQueryVariable("w:P21"), new StaticQueryVariable(standardValue, d)))
    }
    //Also test a value without datatype
    statements.append(new SimpleRDF(new StaticQueryVariable("w:Q76"), new StaticQueryVariable("w:P21"), new StaticQueryVariable(standardValue)))
    for((statement, datatype) <- statements zip datatypes) {
      print(statement.getStatementNt())
      assert(statement.getStatementNt().endsWith("^^<" + datatype + ">."))
    }
    assert(statements.last.getStatementNt() == "<http://www.wikidata.org/entity/Q76> <http://www.wikidata.org/entity/P21> \"%s\".".format(standardValue))
  }
  test("Generate a partially applied simplerdf") {
//    def ignoreProperty(s : String, o : String) : PartialFunction[String, SimpleRDF] = {
//      case (`s`, p, `o`) => p
//    }
//    def waitToCreateRDF(p : String, o: String) : PartialFunction[String, SimpleRDF] = {
////      new SimpleRDF(new StaticQueryVariable(_), new StaticQueryVariable(p), new StaticQueryVariable(p))
//      new SimpleRDF(_, new StaticQueryVariable(p), new StaticQueryVariable(p))
//
//}
//    val partial : PartialFunction[StaticQueryVariable, SimpleRDF] = new SimpleRDF(_, new StaticQueryVariable("p"), new StaticQueryVariable("o"))

    def tset(s : => String) : SimpleRDF = {
      return new SimpleRDF(new StaticQueryVariable(s),new StaticQueryVariable("p"), new StaticQueryVariable("p") )
    }
    def ab : String = return "b"
    val pFunction = new SimpleRDF(_: StaticQueryVariable, _ : StaticQueryVariable, _ : StaticQueryVariable)
    val gQVar = new StaticQueryVariable(_ : String)

    print(tset(ab))
    print(pFunction(gQVar("s"), new StaticQueryVariable("s"), new StaticQueryVariable("s")))
//    val rdf = partial(new StaticQueryVariable("s"))
//    print(rdf)

//    def getSimpleRdfSubject(staticQueryVariableP: StaticQueryVariable, staticQueryVariableO: StaticQueryVariable) : PartialFunction[String, SimpleRDF] = {
//      return new SimpleRDF(new StaticQueryVariable(_), staticQueryVariableP, staticQueryVariableO)
//    }
  }


}
