package rdf

/**
  * Created by Espen on 07.11.2016.
  */
object WikidataPropertyHelper {
  def getId(property : String) : Option[Int] = {
    val idIndex: Int = property.indexOf('P')
    if(idIndex == -1) None
    else return Some(property.substring(idIndex + 1).toInt)
  }
}
