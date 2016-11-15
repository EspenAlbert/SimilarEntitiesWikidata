package query.filters

import query.variables.DynamicQueryVariable

/**
  * Created by Espen on 07.11.2016.
  */
class NotEqualFilter(val dynamicQueryVariable: DynamicQueryVariable, val equalName : String) extends QueryFilter{

  override def getSparql: String = {
    return """filter(%s != %s)""".format(dynamicQueryVariable.getSelectPhrase, equalName)
  }
}
