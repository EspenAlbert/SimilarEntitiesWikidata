package query.variables

/**
  * Created by Espen on 07.11.2016.
  */
trait QueryVariable{
  def getSelectPhrase : String
  def getWherePhrase : String

}
