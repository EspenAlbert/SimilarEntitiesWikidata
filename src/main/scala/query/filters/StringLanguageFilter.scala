package query.filters

import query.variables.DynamicQueryVariable

/**
  * Created by Espen on 07.11.2016.
  */
class StringLanguageFilter(val dynamicQueryVariable: DynamicQueryVariable,  val equalName : String, val languageEqual : String = "en") extends QueryFilter{

  override def getSparql: String = {
    return """filter(%s = "%s"@%s)""".format(dynamicQueryVariable.getSelectPhrase, equalName, languageEqual)
  }
}
