package ownOntologyPopularizer

import org.scalatest.FunSuite

/**
  * Created by Espen on 01.11.2016.
  */
class INeedToBeTestedTest extends FunSuite{
  test("sayHello method works correctly") {
    val hello = new INeedToBeTested()
    assert(hello.sayHello("Scala") == "Hello, Scala!")
  }
  test("my other test returns 1") {
    val hello = new INeedToBeTested
    assert(hello.myOtherTest() == 1)
  }
}
