package core.query.variables

import scala.util.{Failure, Success, Try}

/**
  * Created by Espen on 14.11.2016.
  */
class ResultVariable(pvalue : String) {
  val value = pvalue.trim()
}
object ResultVariable {

  //  val valueInsideQuotes = """\\"[^\\"]*\\"""".r
  val valueInsideQuotes = """"[^"]*"""".r
  val insideNode = """<[^>]*>""".r
  val datatypeAnswer = """[^<]*\^\^<[^>]*>""".r
//  val booleanDatatype = "http://www.w3.org/2001/XMLSchema#boolean"

  implicit def getDouble(x: ResultVariable): Double = {
    try {
      return x.value.toDouble
    } catch {
      case _ => valueInsideQuotes.findFirstIn(x.value) match {
        case Some(v) => return v.substring(1, v.length - 1).toDouble
        case None => throw new Exception(s"failed to convert ${x.value} to double")
      }
    }
  }
  implicit def getBoolean(x : ResultVariable) : Boolean = {
    try {
      return x.value.toBoolean
    } catch {
      case _ => valueInsideQuotes.findFirstIn(x.value) match {
        case Some(v) => return v.substring(1, v.length - 1).toBoolean
        case None => throw new Exception(s"failed to convert ${x.value} to boolean")
      }
    }
  }

  implicit def getInt(x : ResultVariable) : Int = {
    try {
      return x.value.toInt
    } catch {
      case _ => valueInsideQuotes.findFirstIn(x.value) match {
        case Some(v) => Try(v.substring(1, v.length - 1).toInt) match {
          case Success(s) => s
          case Failure(e) => throw new Exception(s"failed to convert ${x.value} to integer")
        }
        case None => throw new Exception(s"failed to convert ${x.value} to integer")
      }
    }
  }
  implicit def getListInt(x : List[ResultVariable]) : List[Int] = {
    for(v <- x) yield getInt(v)
  }
  implicit def getListDouble(x : List[ResultVariable]) : List[Double] = {
    for(v <- x) yield getDouble(v)
  }
  implicit def getListBoolean(x : List[ResultVariable]) : List[Boolean] = {
    for(v <- x) yield getBoolean(v)
  }
  implicit def getString(x : ResultVariable) : String = {
    datatypeAnswer.findFirstIn(x.value) match {
      case Some(v) => return v
      case None => insideNode.findFirstIn(x.value) match {
        case Some(v) => return v.substring(1, v.length - 1)
        case None => valueInsideQuotes.findFirstIn(x.value) match {
          case Some(v) => return v.substring(1, v.length - 1)
          case None => return x.value // throw new Exception(s"Weird result! ${x.value}")
        }
      }
    }
  }
  implicit def getListString(x : List[ResultVariable]) : List[String] = {
    for(v <- x) yield getString(v)
  }
  implicit def getListString(x : Seq[List[ResultVariable]]) : Seq[List[String]] = {
    for(v <- x) yield getListString(v)
  }
}