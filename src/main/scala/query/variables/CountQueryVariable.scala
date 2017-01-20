package query.variables

/**
  * Created by Espen on 07.11.2016.
  */

case class CountQueryVariable(val name : String, val distinct: Boolean, val countVariable : DynamicQueryVariable) extends ResultQueryVariable{
  def getSelectPhrase: String = {
    if(distinct) return s" (count(${countVariable.getSelectPhrase}) as ?$name) \n" else return s" (count(${countVariable.getSelectPhrase}) as ?$name) \n"
  }

  def getWherePhrase: String = {
    countVariable.getWherePhrase
  }
}
