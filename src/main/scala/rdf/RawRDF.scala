package rdf

/**
  * Created by Espen on 09.11.2016.
  */
case class RawRDF(s : String, p : String, o : String) {

}
object RawRDF {
  def ignoreProperty(s : String, o : String) : PartialFunction[Tuple3[String, String, String], String] = {
    case (`s`, p, `o`) => p
  }
  def getParitalFunction(s : String, o : String) : PartialFunction[(String, String, String), String] = {
    return ignoreProperty(s, o)
  }
}