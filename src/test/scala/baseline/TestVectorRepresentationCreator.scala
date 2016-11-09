package baseline

import globals.Namespace
import org.scalatest.FunSuite

/**
  * Created by Espen on 02.11.2016.
  */
class TestVectorRepresentationCreator extends FunSuite{
  test("No errors when creating representation") {
    val representation = VectorRepresentationCreator.createVectorRepresentation(Namespace.w.toString + "Q76")
    print(representation(31))
  }
  test("Compare obama to hillary clinton and george W bush") {
    val prefix = Namespace.w.toString
    VectorRepresentationCreator.compareVectorRepresentationForEntities(prefix + "Q76", prefix + "Q6294", prefix + "Q207", prefix + "Q436113", prefix + "Q22686", prefix + "Q8023")
    print("Yes!!")
  }


}
