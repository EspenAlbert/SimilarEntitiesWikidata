package dataset

import org.scalatest.FunSuite

/**
  * Created by espen on 03.02.17.
  */
class TestSubArtistDataset extends FunSuite {

  test("Able to create a dataset") {
    val filtered = CreateSubArtistDataset.produceDataset()
    for ((key, values) <- filtered) {
      if (values.length > 9) {
        println(key, ":")
        println(values)

      }
    }

  }

}