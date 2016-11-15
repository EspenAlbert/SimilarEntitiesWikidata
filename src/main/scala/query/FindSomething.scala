package query

import java.io.ByteArrayOutputStream

import query.variables.{ResultQueryVariable, ResultVariable}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
trait FindSomething{
  val resultStream : ByteArrayOutputStream
  var resList : String = null
  var headers : ArrayBuffer[String] = null
  var values : ArrayBuffer[ResultVariable] = null

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

  def getResults(variable: ResultQueryVariable): List[ResultVariable] = {
    if(resList == null) resList =resultStream.toString
    assert(resList.length > 0)
    if(headers == null) headers = findHeaders(resList.split("\\|").drop(1))
    val index = headers.indexOf(variable.name)
    assert(index != -1)
    if(values== null) getValues
    def getValues: Unit = {
      val rawValues = new ArrayBuffer[String]()
      for (line <- resList.split("\r\n").drop(3).dropRight(1)) {
        //First three lines are headers last line is not a result
        rawValues.append(line.split("\\|").drop(1): _*)
      }
      values = new ArrayBuffer[ResultVariable]()
      for(value <- rawValues) {
        values.append(new ResultVariable(value))
      }
    }


    return (for(
      (value,i) <- values.zipWithIndex
      if i % headers.length == index) yield value).toList
  }
}
