package randomPlay

import core.globals.KnowledgeGraphs
import core.testData.WikidataFactory
import org.scalatest.FunSuite
import tags.ActiveTag

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex.MatchIterator

/**
  * Created by espen on 16.03.17.
  */
class TestScalaTestPlay extends FunSuite{
  def fixture = new {
    val builder = new StringBuilder("Scala test is")
    val buffer = new ListBuffer[String]
  }
  test("testing fixture", ActiveTag) {
    val f = fixture
    f.builder.append(" easy")
    assertResult("Scala test is easy"){f.builder.toString()}
    assert(f.buffer.isEmpty)
    f.buffer.append("test")
    assert(f.buffer.nonEmpty)
  }
  test("fixture reset after previous test", ActiveTag) {
    val f = fixture
    assert(f.buffer.isEmpty)
  }
  test("iterating over null should work"){
    var eIsObject : (List[String], List[String]) = (Nil, Nil)
    for(a <- eIsObject._1) {
      println(a)
    }
  }
  test("fold left starts from head?") {
    val list = 1 to 9
    println(list.foldLeft(0){case(acc, nextValue) => {
      println(s"Next value $nextValue")
      acc + nextValue
    }})
  }
  test("Abbreviate a name") {
    val rStarr = "Ringo Starr"
    val threeLettersFirstNameTwoLettersLastname = """(\w{3})[^\s]* (\w{2})""".r
    matchPrinter(rStarr)
    val wd = WikidataFactory
    List(wd.ringoStarr.id, wd.countryProp, "Espen Albert").foreach(matchPrinter)
    threeLettersFirstNameTwoLettersLastname.findAllIn(rStarr) match {
      case a : MatchIterator if(a.nonEmpty) =>
        val listOfMatches = a.matchData.map(_.subgroups).toList
        println(listOfMatches);
      case _ => println("no match")
    }

  }

  private def matchPrinter(label: String) = {
    label.split(" ") match {
      case a: Array[String] if a.size < 5 && a.size > 2 => println(a.map(_.head.toUpper).mkString(""))
      case a: Array[String] if a.size < 3 && a.size > 1 => println(s"${a.head.take(3)}${a(1).take(2)}")
      case a if a.size == 1 => label match {
        case b if b.indexOf("P") > 0 => println(b.substring(b.indexOf("P")))
        case b if b.indexOf("Q") > 0 => println(b.substring(b.indexOf("Q")))
      }
      case _ => println(s"no match $label")
    }
  }
  test("Proper splitting of results" ){
    val errorCreator = "5134512F|5134113F"
    val shouldSplit = "normal | 5134113F"
    val splitted = errorCreator.split("""\s\|\s""")
    val splitted2 = shouldSplit.split("""\s\|\s""")
    println(splitted.mkString("\n"))
    println(splitted2.mkString("\n"))

  }

}
