package query

/**
  * Created by Espen on 02.11.2016.
  */
trait FindProperty extends FindSomething{
  def getProperties(): List[String] = {
    return (for(s <- findVariable('p')) yield s.dropRight(1)).toList
  }

}
