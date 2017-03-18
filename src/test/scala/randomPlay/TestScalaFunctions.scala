package randomPlay

import org.scalatest.FunSuite
import tags.{ActiveTag, TestOnlyTag}

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
}
