package query.variables

/**
  * Created by Espen on 07.11.2016.
  */

class CountQueryVariable(val name : String, val distinct: Boolean, val countVariable : DynamicQueryVariable) extends ResultVariable{
  def getSelectPhrase: String = {
    if(distinct) return s"select (count(distinct ${countVariable.getSelectPhrase}) as ?$name) \n" else return s"select (count(${countVariable.getSelectPhrase}) as ?$name) \n"
  }
}
