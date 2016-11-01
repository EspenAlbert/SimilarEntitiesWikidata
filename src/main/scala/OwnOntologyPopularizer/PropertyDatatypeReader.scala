package OwnOntologyPopularizer

import scala.io.Source

/**
  * Created by Espen on 01.11.2016.
  */
class PropertyDatatypeReader {
  def getPropertyDatatypeMap(): collection.mutable.Map[String, String] = {
    val propertyDatatype = new collection.mutable.HashMap[String, String]()
    val linesFromFile = Source.fromFile("input/propertiesAndDatatype.txt").getLines()
    for(line <- linesFromFile) {
      val property = line.substring(0, line.indexOf('|'))
      val datatype = line.substring(line.indexOf('|')+1)
      propertyDatatype(property) = datatype
    }
    return propertyDatatype
  }

}
