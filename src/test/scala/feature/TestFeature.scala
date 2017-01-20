package feature

import globals.FeatureType
import org.scalatest.FunSuite

/**
  * Created by espen on 17-Dec-16.
  */
class TestFeature extends FunSuite{
  test("a feature is able to find its label") {
    val feature = new Feature("http://www.wikidata.org/entity/Q76", FeatureType.sameProperty, 2, 21.2)
    println(feature)
  }

}
