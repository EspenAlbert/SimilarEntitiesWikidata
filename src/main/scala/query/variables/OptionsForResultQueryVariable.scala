package query.variables

/**
  * Created by Espen on 04.11.2016.
  */
object OptionsForResultQueryVariable extends Enumeration{


  type OptionsForResultQueryVariable = Value
  val count = Value("count")
  val distinct = Value("distinct")
  val sameTypeFilter = Value("sameTypeFilter")
  val unknownTypeFilter = Value("unknownTypeFilter")
  val sameLanguageFilter = Value("sameLanguageFilter")
  val notEqualFilter = Value("notEqualFilter")
  val ignoreMe = Value("ignore")

  implicit def getStringFromOptionsForResultQueryVariable(optionsForResultQueryVariable: OptionsForResultQueryVariable) : String = {
    return optionsForResultQueryVariable.toString
  }

}
