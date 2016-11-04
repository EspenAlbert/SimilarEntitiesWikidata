package query

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
trait FindSomething extends Query{
  var resList : String = null
  var headers : ArrayBuffer[Char] = null
  var values : ArrayBuffer[String] = null

  def findVariables(iterable: Iterable[String]) : ArrayBuffer[Char] = {
    val headers = new scala.collection.mutable.ArrayBuffer[Char]()

    for(header <- iterable) {
      val trimmedHeader = header.trim()
      if(header.startsWith("\r\n")) return headers
      else if(trimmedHeader.length > 1) throw new Exception("A header value with more than one character!")
      else headers.append(trimmedHeader.charAt(0))
    }
    throw new Exception("Unknown headers")

  }

  def findVariable(char: Char): ArrayBuffer[String] = {
    if(resList == null) resList =resultStream.toString
    assert(resList.length > 0)
    if(headers == null) headers = findVariables(resList.split("\\|").drop(1))
    val index = headers.indexOf(char)
    assert(index != -1)
    if(values== null) getValues
    def getValues: Unit = {
      values = new scala.collection.mutable.ArrayBuffer[String]()
      for (line <- resList.split("\r\n").drop(3).dropRight(1)) {
        //First three lines are headers last line is not a result
        values.append(line.split("\\|").drop(1): _*)
      }
    }

    return for(
      (value,i) <- values.zipWithIndex
      if i % headers.length == index) yield value.trim()

  }
}
