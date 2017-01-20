package math

import org.scalatest.FunSuite
import breeze.linalg._

/**
  * Created by Espen on 02.11.2016.
  */
class TestBreezeLibrary extends FunSuite{
  test("testing library") {
    val x = DenseVector.zeros[Double](5)
    print(x.length)
    val y = DenseVector(1d,2d,3d)
    y(1 to 2) := 0.5
    print(y)

    val a = DenseVector(1d, 2d)
    val b = DenseVector(1d, 3d)
    print(a :* b)
    print(a :* a)
    print(a dot b, a.t * b)
    a(0) += 50
    print(a)

    print("Sum" + sum(a))

  }
}
