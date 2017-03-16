package iAndO.readRdf

import java.io.{BufferedInputStream, FileInputStream, InputStream, PrintWriter}
import java.util.zip.GZIPInputStream

import core.globals.MyDatasets
import core.query.specific.UpdateQueryFactory
import iAndO.readRdf.bigDataset.SplitAndFixRDFBigHelper.uploadToDataset
import org.apache.jena.query.QueryParseException
import org.apache
import org.apache.commons.compress.compressors.CompressorStreamFactory

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by espen on 13.03.17.
  */
object ReadDBpedia {
  def main(args: Array[String]) {
    //    var lines = Source.fromFile("/home/espen/prog/java/Wikidata-Toolkit-master/results/wikidata-statements.nt.gz", enc = "utf-8")
    def gis(s: String): InputStream = return new CompressorStreamFactory().createCompressorInputStream(new BufferedInputStream(new FileInputStream(s)))

    var fileNumber = 0
    var lines = Source.fromInputStream(gis("/media/espen/Windows8_OS/dataset/dbPedia/page_links_en.ttl.bz2"), enc = "utf-8")
    var s = new StringBuilder()
    var iter = lines.iter
    val uploadEvery = 10000
    var i = 0
    var printWriter = new PrintWriter(s"input/errorLog$fileNumber.txt")
    var errorNumber = 1
    val alreadyParsedLines = 162440000
    var statements = ListBuffer[String]()
    //    val alreadyParsedLines = 1510000
    while (iter.hasNext) {
      iter.next() match {
        case '\n' => {
          i += 1
          if (i > alreadyParsedLines) {
            statements.append(s.toString())
            if (i % uploadEvery == 0) {
              println(s"upload line nr: $i")
              try {
                UpdateQueryFactory.addStatements(statements, MyDatasets.DBpediaDS)
                statements.clear()
              } catch {
                case a: QueryParseException => {
                  printWriter.write(s"Line $i : ${s.toString()} had ERROR NR $errorNumber : ${a.getMessage} \n\n\n\n statements: ${statements.mkString("\n")}")
                  println(a.getMessage)
                  errorNumber += 1
                  if (errorNumber % 10 == 0) {
                    printWriter.close()
                    fileNumber += 1
                    printWriter = new PrintWriter(s"input/errorLogDBpedia$fileNumber.txt")
                  }
                }
                case a: Throwable => {
                  printWriter.write(s"Line $i : ${s.toString()} had OTHER TYPE OF ERROR: ERROR NR $errorNumber : ${a.getMessage} \n\n\n\n statements: ${statements.mkString("\n")}")
                  println(a.getMessage)
                  errorNumber += 1
                  if (errorNumber % 10 == 0) {
                    printWriter.close()
                    fileNumber += 1
                    printWriter = new PrintWriter(s"input/errorLogDBpedia$fileNumber.txt")
                  }
                }
              }
            }
          }
          s.clear()
        }
        case c => s.append(c)
      }
    }
    println(s"In total $i number of lines")
  }

}
