package dump

import java.io.File

import com.lambdaworks.jacks.JacksMapper
import org.apache.commons.io.FileUtils

/**
  * Created by Espen on 09.11.2016.
  */

object DumpObject {
  def dumpJsonMapStringListString(stringToStrings: Map[String, List[String]], filename : String): Unit = {
    val json = JacksMapper.writeValueAsString[Map[String, List[String]]](stringToStrings)
    val fullFilename: String = picklePath + filename +".txt"
    FileUtils.write(new File(fullFilename), json)
  }

  final val picklePath = "output/pickles/"
  def dumpJsonMap(objectToDump : Map[String, Double], filename : String) : Unit = {
    val fullFilename: String = picklePath + filename +".txt"
    //    test1.thePickle.
    val json = objectToDump match {
      case m : Map[String, Double] => JacksMapper.writeValueAsString[Map[String, Double]](m)
      case s => print(s); throw new Exception("Failed")
    }
    FileUtils.write(new File(fullFilename), json)
  }
  def loadJsonMap(filename : String) : Map[String, Double] = {
    val json = FileUtils.readFileToString(new File(picklePath + filename + ".txt"))
    val map = JacksMapper.readValue[Map[String, Double]](json)
    return map
  }
  def getStringMap(filename : String) : Map[String, List[String]] = {
    val json = FileUtils.readFileToString(new File(picklePath + filename + ".txt"))
    val map = JacksMapper.readValue[Map[String, List[String]]](json)
    return map
  }
}
