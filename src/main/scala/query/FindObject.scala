package query

/**
  * Created by Espen on 02.11.2016.
  */
trait FindObject extends FindSomething{



  def getObjects(): List[String] = {
    return findVariable('o')
  }

}
