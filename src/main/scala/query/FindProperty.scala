package query

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Espen on 02.11.2016.
  */
trait FindProperty extends FindSomething{
  def getProperties(): ArrayBuffer[String] = {
    return findVariable('p')
  }

}
