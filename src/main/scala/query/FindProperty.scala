package query

/**
  * Created by Espen on 02.11.2016.
  */
trait FindProperty extends Query{
  def getProperties(): List[String] = {
    val resList : String = resultStream.toString()
    return null
  }

}
