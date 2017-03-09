package iAndO.readRdf

import java.io.PrintWriter

import scala.io.Source
import scala.util.matching.Regex.MatchIterator

/**
  * Created by Espen on 13.10.2016.
  */
object SplitAndFixRDF {
  val NON_FIXABLE_URI = "ERROR"
  final val StatementsPerFile: Int = 100000
  val totalPattern = ("<http://[^>]*(" +
    "(\\\\\")|" +
    "(\\^)|" +
    "(\\|)|" +
    "(\\})|" +
    "(\\`)|" +
    "(\\{)|" +
    "(\\\\n)|" +
    "(\\\\\\\\)|" +
    "(\\s))" +
    "").r
  val bracketsPattern = "(\\{[^>]*\\})".r
  val subPattern = "(\\\\\")|(\\^)|(\\|)|(\\})|(\\`)|(\\{)|(\\\\n)|(\\{[^>]*\\})|(\\\\\\\\)|(\\s)".r
  val propertyEndWithCPattern = "<http://www.wikidata.org/entity/P\\d{1,4}[^>]*>".r

  var stringBuffer = new StringBuilder()
  var counter = 0
  var fileNumber = 1

  def main(args: Array[String]) {
    println("Following is the content read:")
    var lines = Source.fromFile("C:/dataset/20160801/wikidata-simple-statements.nt", enc = "utf-8").getLines()
    for (line <- lines) {
      appendBuffer(line)
    }
  }

  def appendBuffer(line: String): Unit = {
    counter += 1
    if (counter % StatementsPerFile == 0) {
      val printWriter = new PrintWriter("C:/dataset/smallFiles2/wikidata-simple-statements" + fileNumber + ".nt")
      fileNumber += 1
      printWriter.write(stringBuffer.toString())
      printWriter.close()
      stringBuffer.clear()
      println(s"@ file number: $fileNumber")
    }
    val cleanedLine = cleanUpLine(line)
    if(cleanedLine != NON_FIXABLE_URI) stringBuffer.append(cleanedLine + "\n")
  }
  def cleanUpLine(line: String) : String = {
    val matches: MatchIterator = totalPattern.findAllIn(line)
    if (!matches.hasNext) {
      return removeCAtTheEndOfProperties(line)
    }
    else {
      val modifiedLine = fixURI(line, matches)
      print("Modified line: " + modifiedLine)
      if (modifiedLine != NON_FIXABLE_URI) {
        return removeCAtTheEndOfProperties(modifiedLine)
      }
      else return modifiedLine
    }
  }

  //Remove c from the end of properties, eg: <http://www.wikidata.org/entity/Q4537983> <http://www.wikidata.org/entity/P1687c> <http://www.wikidata.org/entity/P1553>
  def removeCAtTheEndOfProperties(line: String): String = {
    val matches: MatchIterator = propertyEndWithCPattern.findAllIn(line)
    val listMatches = matches.toList
    if (listMatches.length == 0) return line
    val modifiedProperty = fixProperty(listMatches(0))
    return line.replace(listMatches(0), modifiedProperty)
  }

  def fixProperty(prop: String): String = {
    var cToRemove = 1
    for (c <- prop.dropRight(1).reverse) {
      if (c.isDigit) return prop.dropRight(cToRemove) + ">"
      cToRemove += 1
    }
    throw new Exception("Unable to fix prop: " + prop)
  }

  def fixURI(line: String, matches: MatchIterator): String = {
    var newURI = ""
    for (oldURI <- matches) {
      //println("error URI:" + oldURI)
      val subMatches = subPattern.findAllIn(oldURI)
      var counter = 0
      for (subValue <- subMatches) {
        subValue match {
          case "{" => println("removing {"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case "}" => println("removing }"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case "`" => println("removing `"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "%60") else oldURI.replaceAllLiterally(subValue, "%60")
          case "|" => println("removing `"); return NON_FIXABLE_URI
          case "^" => println("removing `"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "%5E") else oldURI.replace(subValue, "%5E")
          case "\\\"" => println("removing \\\""); return NON_FIXABLE_URI
          case "\\\\" => println("removing \\\\"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case "\\n" => println("removing \\n"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "") else oldURI.replaceAllLiterally(subValue, "")
          case " " => println("removing (space)"); newURI = if (counter > 0) newURI.replaceAllLiterally(subValue, "%20") else oldURI.replaceAllLiterally(subValue, "%20")
          case a => println("not removing...." + a)
        }
        counter += 1
      }
//      if (bracketsPattern.findAllIn(line).hasNext) {
//        return NON_FIXABLE_URI
//      }
      return line.replace(oldURI, newURI)
    }
    throw new Exception("Found an unknown pattern...")
  }
}

