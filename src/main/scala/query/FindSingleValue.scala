package query

/**
  * Created by Espen on 02.11.2016.
  */
trait FindSingleValue extends FindSomething{
  def getValue(): String = {
    val values = findVariable('v')
    assert(values.length == 1)
    if(values(0).startsWith("<")) return values(0).drop(1).dropRight(1)
    return values(0)
  }

}
