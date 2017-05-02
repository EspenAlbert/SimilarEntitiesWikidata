package iAndO.readRdf.bigDataset

import scala.util.matching.Regex.MatchIterator

/**
  * Created by espen on 16.02.17.
  */
object URIFixer {
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

  def fixFixableURIs(lines : Iterable[String]) : Iterable[String] = {
    return for(l <- lines;
      cleanedLine = cleanUpLine(l) match {
        case Some(fixed) => fixed
        case None => NON_FIXABLE_URI
      }
      if(cleanedLine != NON_FIXABLE_URI))yield cleanedLine

  }

  def appendBuffer(line: String): Unit = {
    val cleanedLine = cleanUpLine(line)
    if (cleanedLine != NON_FIXABLE_URI) stringBuffer.append(cleanedLine + "\n")
  }

  def cleanUpLine(line: String): Option[String] = {
    val matches: MatchIterator = totalPattern.findAllIn(line)
    if (!matches.hasNext) {
      return Some(line)
    }
    else {
      val modifiedLine = fixURI(line, matches)
      print("Modified line: " + modifiedLine)
      if (modifiedLine != NON_FIXABLE_URI) {
        return Some(modifiedLine)
      }
      else return None
    }
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