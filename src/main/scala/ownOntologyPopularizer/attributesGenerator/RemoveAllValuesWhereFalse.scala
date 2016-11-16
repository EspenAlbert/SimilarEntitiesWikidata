package ownOntologyPopularizer.attributesGenerator

import java.io.PrintWriter

import scala.io.Source

/**
  * Created by Espen on 16.11.2016.
  */
object RemoveAllValuesWhereFalse {
  def removeBooleansWhereFalse(filename : String ): Unit = {
    val lines = Source.fromFile("output/" + filename + ".nt").getLines().toList
    val printWriter = new PrintWriter("output/" + filename + ".nt")
    for(l <- lines) {
      if(!l.contains("false")) printWriter.write(l + "\n")
    }
    printWriter.close()

  }
}
