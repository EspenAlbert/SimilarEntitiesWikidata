package core.dump

/**
  * Created by espen on 15.06.17.
  */
import java.io.{File, FileWriter}

import buildInfo.BuildInfo
import core.globals.KnowledgeGraphs
import core.globals.KnowledgeGraphs.KnowledgeGraph
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.io.Source
object DumpJson {

  def main(args: Array[String]): Unit = {
//    readCustomJson()
  }
  final val picklePath = BuildInfo.similarEntitiesCorePath + "/output/pickles/"

  def writeToFile[T](value : T, filename : String)(implicit writer : Writes[T]) : Unit = {
    val json = Json.toJson(value)
    val fileWriter = new FileWriter(picklePath + filename)
    fileWriter.write(json.toString())
    fileWriter.close()
  }

  def readFromFile[T](filename: String)(implicit reader : Reads[T]) : Option[T] = {
    val rawText = Source.fromFile(picklePath + filename).getLines().mkString("\n")
    val json = Json.parse(rawText)

    json.validate[T] match {
      case s: JsSuccess[T] => {
        val value = s.get
        Some(value)
      }
      case e : JsError => {
        println(e)
        None
      }
    }
  }


}
