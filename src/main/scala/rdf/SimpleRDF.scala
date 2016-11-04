package rdf

/**
  * Created by Espen on 04.11.2016.
  */
class SimpleRDF(val s: String, val p: String, val o: String) {
  def getStatementNt(): String = {
    return "<" + s + "> <" + p + "> <" + o + ">"
  }

}
