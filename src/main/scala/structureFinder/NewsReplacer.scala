package structureFinder

import core.globals.KnowledgeGraph
import rssFeed.{JsonParser, NewsReader}
import rssFeed.JsonParser.findMainEntityAndOtherEntities

/**
  * Created by espen on 28.04.17.
  */
object NewsReplacer {
  def main(args: Array[String]): Unit = {
    val filenames = List(
            "North_Korea_faces_tighter_sanctions_under_Trump_strategy.txt"
      //      "Trump_won't_scrap_Nafta_trade_deal_'at_this_time'.txt"
//      "Brides_wanted.txt"
//      "Russia_ramps_up_its_military_in_the_Arctic.txt"
    )
    val trumpAndKimgJong = List("http://www.wikidata.org/entity/Q22686", "http://www.wikidata.org/entity/Q56226")
//    testArticle(filenames)
    val paths = KConnectivitySparqlBuilder.findPathsBetweenEntities(trumpAndKimgJong)
    paths.foreach(println)
  }


  private def testArticle(filenames: List[String]) = {
    val filename = "Brides_wanted.txt"
    val activeKG = KnowledgeGraph.wikidata
    val kgPrefix = KnowledgeGraph.getDatasetEntityPrefix(activeKG)
    for (f <- filenames) {
      val originalSummary = NewsReader.getSummary(f)
      val mainEntity :: restOfEntities = JsonParser.findMainEntityAndOtherEntities(f)
      val entityURIs = mainEntity._2 :: restOfEntities.map(_._2).filter(e => e.startsWith(kgPrefix) && e != mainEntity._2)
      println(s"Mapped uris: $entityURIs")
      KConnectivitySparqlBuilder.pathMaxLength = 3
      val topPaths = KConnectivitySparqlBuilder.findPathsBetweenEntities(entityURIs)
      topPaths.foreach(println)
      println(s"""['${topPaths.mkString("""', '""")}']""")

    }
  }
}
