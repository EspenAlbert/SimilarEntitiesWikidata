package rdf

/**
  * Created by Espen on 07.11.2016.
  */
object WikidataPropertyHelper {
  def getId(property : String) : Int = {
    val idIndex: Int = property.indexOf('P')
    if(idIndex == -1) throw new Exception("couldn't find an id for property: " + property)
    else return property.substring(idIndex + 1).toInt
  }
}
