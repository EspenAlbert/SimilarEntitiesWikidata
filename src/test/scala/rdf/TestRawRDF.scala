package rdf

import org.scalatest.FunSuite

import scala.collection.mutable

/**
  * Created by Espen on 02.11.2016.
  */
class TestRawRDF extends FunSuite{
  test("one should be able to pattern match") {
    val statement = RawRDF("s", "p", "o")
    val matchSubject = statement match {
      case RawRDF(s, p, o) => o
    }
    assert(matchSubject == statement.o)
    val statement2 = RawRDF("s2", "p", "o")
    assert(statement != statement2)

    val matchObject = statement2 match {
      case RawRDF(s, p, "o") => true
    }
    assert(matchObject)
//    val statement3 = RawRDF(_, "p", "o")("s")
    val partialMatch = statement match {
      case RawRDF(_, p, o) => true
      case _ => false
    }
//    assert(statement3("s") == statement)
    assert(partialMatch)

    val myPartialFunction = RawRDF.getParitalFunction("s", "o")
    assert(!myPartialFunction.isDefinedAt(("s1", "p", "o")))


//    assert(RawRDF.ignoreProperty.isDefinedAt(List("s", "jalla", "o")))
//    assert(RawRDF.ignoreProperty(List("s", "jalla", "o")) == "jalla")
  }
  test("Playing around with matching on sets") {
    val statements = mutable.Set[Tuple3[String, String, String]]()
    statements.add(("s", "p", "o"))
    statements.add(("s", "p2", "o"))
    val properties = statements.filter(_ == ("s", "p", "o"))
    val properties2 = statements.filter((x) => x._1 == "s")
    print(properties)
  }


}
