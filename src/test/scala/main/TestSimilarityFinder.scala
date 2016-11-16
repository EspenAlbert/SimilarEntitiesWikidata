package main

import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestSimilarityFinder extends FunSuite{
  test("must be a lot of errors!!! ???") {
    SimilarityFinder.findTopKSimilarTo("w:Q76", 10)
  }
  test("similarities for tufte Q436113") {
    SimilarityFinder.findTopKSimilarTo("w:Q436113", 10)
  }
}
