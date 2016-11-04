package query

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
trait FindObject extends FindSomething{



  def getObjects(): ArrayBuffer[String] = {
    return findVariable('o')
  }

}
