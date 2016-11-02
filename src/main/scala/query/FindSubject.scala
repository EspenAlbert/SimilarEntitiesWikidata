package query

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
trait FindSubject extends FindSomething{



  def getSubjects(): ArrayBuffer[String] = {
    return findVariable('s')
  }

}
