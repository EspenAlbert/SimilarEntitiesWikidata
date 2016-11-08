package query

import java.io.ByteArrayOutputStream

import query.variables.ResultVariable

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
trait FindSomething{
  val resultStream : ByteArrayOutputStream
  var resList : String = null
  var headers : ArrayBuffer[String] = null
  var values : ArrayBuffer[String] = null

  private def findHeaders(iterable: Iterable[String]) : ArrayBuffer[String] = {
    val headers = new scala.collection.mutable.ArrayBuffer[String]()
    for(header <- iterable) {
      val trimmedHeader = header.trim()
      if(header.startsWith("\r\n")) return headers
//      else if(trimmedHeader.length > 1) throw new Exception("A header value with more than one character!")
      else headers.append(trimmedHeader)
    }
    throw new Exception("Unknown headers")

  }

  def getResults(variable: ResultVariable): List[String] = {
    if(resList == null) resList =resultStream.toString
    assert(resList.length > 0)
    if(headers == null) headers = findHeaders(resList.split("\\|").drop(1))
    val index = headers.indexOf(variable.name)
    assert(index != -1)
    if(values== null) getValues
    def getValues: Unit = {
      values = new ArrayBuffer[String]()
      for (line <- resList.split("\r\n").drop(3).dropRight(1)) {
        //First three lines are headers last line is not a result
        values.append(line.split("\\|").drop(1): _*)
      }
      val modifiedValues = new ArrayBuffer[String]()
      for(value <- values) {
        if(value.trim.startsWith("<")) modifiedValues.append(value.trim.drop(1).dropRight(1))
        else if(value.trim.startsWith("\"")) modifiedValues.append(value.trim().drop(1).dropRight(1))
        else modifiedValues.append(value.trim)
      }
      values = modifiedValues
    }


    return (for(
      (value,i) <- values.zipWithIndex
      if i % headers.length == index) yield value.trim()).toList
  }
}
