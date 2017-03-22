package future

/**
  * Created by espen on 21.03.17.
  */
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object FutureReturner {

  def getFuture(a : Int) : Future[Int] ={
    Future {
      Thread.sleep(2000)
      a
    }
  }
}
