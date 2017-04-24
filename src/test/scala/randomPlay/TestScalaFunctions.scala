package randomPlay

import org.scalatest.FunSuite
import tags.{ActiveTag, TestOnlyTag}

import sys.process._
import java.net.{HttpURLConnection, URL}
import java.io._

import scala.io.Source
import scala.util.{Failure, Success, Try}
/**
  * Created by espen on 18.03.17.
  */
class TestScalaFunctions extends FunSuite{
  test("check intersection of two lists") {
    val list1 = List(1, 2, 3)
    val list2 = List(3,4,5)
    val intersect = list1.intersect(list2)
    assert(intersect == List(3))
    //Checking strings
    val list3 = List("1, 2", "3")
    val list4 = List("3","4,5")
    val intersect2 = list3.intersect(list4)
    assert(intersect2 == List("3"))
    val a = Map("a" -> (List(1), "2"))
    val mapped = a.map{case (c, (d, e)) => (c+ e) -> d}
    assert(mapped == Map("a2" -> List(1)))
    val b = Map("b" -> (List(2), "3"))
    for(i <- a.zip(b)) {
      i match {
        case ((c, (d,e)), (f, (g,h))) => assert(h== "3")
      }
    }
    val intersectionOfLists = a.zip(b).map{case ((c, (d,e)), (f, (g,h))) => d.intersect(g).size}
    println(intersectionOfLists)
    assert(intersectionOfLists.head == 0)
  }
  test("filter and map in the same operation didn't work as hoped...", ActiveTag) {
    val listOfSome = List(Some("a"), None, Some("b"))
    val grouped = listOfSome.groupBy(f => f.isDefined)
    println(grouped.getOrElse(true, null))
    println(grouped.getOrElse(false, null))
    assert(List("a", "b") != listOfSome.map{case Some(s) => s; case _ =>})
  }
  val standardOutputImageLocation = "output/images/"
  def fileDownloader(url: String, filename: String) = {
    new URL(url) #> new File(s"$standardOutputImageLocation$filename") !!
  }
  test("Download image from internet") {
    val url = "https://upload.wikimedia.org/wikipedia/commons/8/80/Europe_location_BEL.png"
    val filename = "output/images/test1.png"
    fileDownloader(url, filename)
  }
  test("Download image from internet 2") {
    var out: OutputStream = null;
    var in: InputStream = null;

    try {
      val url = new URL("http://commons.wikimedia.org/wiki/File:Europe_location_BEL.png")

      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      in = connection.getInputStream
      val localfile = "output/images/test1.png"
      out = new BufferedOutputStream(new FileOutputStream(localfile))
      val byteArray = Stream.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray

      out.write(byteArray)
    } catch {
      case e: Exception => println(e.printStackTrace())
    } finally {
      out.close
      in.close
    }
  }
  def findTrueUrlForImageAndStoreToFile(rawUrl : String) : Unit = {
    val s = Source.fromURL(rawUrl.replace("http", "https")).getLines().mkString("\n")
    val findingPattern = if(rawUrl.endsWith(".svg")) """.*(<a href=[^(/a)]*1000px)""".r else """.*(a href=.*Original file)""".r
    val urlPattern = """http[^\"]*[(\.png)|(\.jpg)|(\.svg)]""".r
    findingPattern.findFirstIn(s) match {
      case Some(a) => urlPattern.findFirstIn(a) match {
        case Some(url) => fileDownloader(url, url.substring(url.lastIndexOf("/")))
        case None => println(s"Unable to find $a")
      }
      case None => println(s"not found Original file for $rawUrl")
    }

  }
  test("url downloader from link in dataset") {
//    val s = Source.fromURL("https://commons.wikimedia.org/wiki/File:Europe_location_BEL.png").getLines().mkString("\n")
//    println(s)
//    val s = Source.fromFile("output/images/testFindingCorrectLink.txt").getLines().mkString("\n")
//    val links = List("http://commons.wikimedia.org/wiki/File:Europe_location_BEL.png","http://commons.wikimedia.org/wiki/File:EU-Denmark.svg", "http://commons.wikimedia.org/wiki/File:HubbleTuningFork.jpg")
    val links = List("http://commons.wikimedia.org/wiki/File:EU-Denmark.svg")
    links.foreach(findTrueUrlForImageAndStoreToFile)
  }
  test("flat map option list") {
    val a = List(Try("a"), Failure, Try("b21")).collect{case Success(a) => a}
    println(a)
    println(Try(throw new Exception("a")).getOrElse("lol"))
  }
}
