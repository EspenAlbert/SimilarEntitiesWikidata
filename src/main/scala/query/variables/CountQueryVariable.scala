package query.variables

/**
  * Created by Espen on 07.11.2016.
  */

class CountQueryVariable(val name : String, val distinct: Boolean, val countVariable : DynamicQueryVariable) extends ResultVariable{
  def getSelectPhrase: String = {
    if(distinct) return s"(count(distinct ${countVariable.getSelectPhrase}) as ?$name)" else return s"(count(${countVariable.getSelectPhrase}) as ?$name)"
  }
}
