package query

/**
  * Created by Espen on 02.11.2016.
  */
trait FindSubject extends FindSomething{



  def getSubjects(): List[String] = {
    return findVariable('s')
  }

}
