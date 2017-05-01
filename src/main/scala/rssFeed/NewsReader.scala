package rssFeed

import scala.io.Source

/**
  * Created by espen on 28.04.17.
  */
object NewsReader {

  def getSummary(filename : String) : String = {
    val unDroppedSummary = Source.fromFile("input/news/bbc/" + filename, "utf-8").getLines().toList
    val originalSummarySingleLine = unDroppedSummary.last
    return originalSummarySingleLine
  }

}
