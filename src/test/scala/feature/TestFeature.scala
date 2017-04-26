package feature

import core.feature.{Feature, PathFeature}
import core.globals.FeatureType
import data.WikidataFactory
import org.scalatest.FunSuite

/**
  * Created by espen on 17-Dec-16.
  */
class TestFeature extends FunSuite{
  test("a feature is able to find its label") {
    val feature = new Feature("http://www.wikidata.org/entity/Q76", FeatureType.sameProperty, 2, 21.2)
    println(feature)
  }
  test("PathFeature should create correct paths of length 1 and 2") {
    val wd = WikidataFactory
    val l1FoundE = wd.ringoStarr.spouseValues.head
    val spouseProp = wd.ringoStarr.spouseProp
    val rs = wd.ringoStarr.id
    val expected = s"${rs},true,${spouseProp},${l1FoundE}"
    val actual = PathFeature.createPathLength1(rs, spouseProp, true, l1FoundE)
    assert(expected == actual)
    val memberProp = wd.ringoStarr.memberOfProp
    val beatles = wd.ringoStarr.memberOfValue
    val jl = wd.johnLennon
    val expectedL2 = s"$rs,true,${memberProp},${beatles},false,${memberProp},${jl}"
    val actualL2 = PathFeature.createPathLength2(rs, true, memberProp, beatles, false, memberProp, jl)
    assert(expectedL2 == actualL2)

  }

}
