package baseline

import query.specific.{FindAllDistinctEntitiesQuery, FindAllDistinctPropertiesQuery, FindSubjectsWithProperty}

/**
  * Created by Espen on 07.11.2016.
  */
object PropertyIdfCalculator {
  def getMap() : Map[String, Float] = {
    val queryForProperties = new FindAllDistinctPropertiesQuery()
    queryForProperties.execute()
    val queryForAllEntities = new FindAllDistinctEntitiesQuery()
    queryForAllEntities.execute()
    val numberOfEntitiesInWikidata = queryForAllEntities.getSubjects().length
    val properties = queryForProperties.getProperties()
    for(prop <- properties) {
      val querySubjectsWithProperty = new FindSubjectsWithProperty(prop)
      querySubjectsWithProperty.execute()

    }
    return null
  }

}
