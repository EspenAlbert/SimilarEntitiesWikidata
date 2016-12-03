package query.variables

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
  implicit def getInt(x : ResultVariable) : Int = {
    try {
      return x.value.toInt
    } catch {
      case _ => valueInsideQuotes.findFirstIn(x.value) match {
        case Some(v) => return v.substring(1, v.length - 1).toInt
        case None => throw new Exception(s"failed to convert ${x.value} to integer")
      }
    }
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