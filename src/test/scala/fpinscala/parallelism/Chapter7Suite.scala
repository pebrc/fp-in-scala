package fpinscala.parallelism

import org.scalatest.{ BeforeAndAfterAll, FunSuite }
import org.scalatest.Matchers._
import java.util.concurrent._
import Par._

class Chapter7Suite extends FunSuite with BeforeAndAfterAll {

  private val es = Executors.newCachedThreadPool()

  test("map2WithTimeouts respects time outs") {
    def tooLong = {
      TimeUnit.MILLISECONDS.sleep(70)
      ()
    }
    def justRight = {
      TimeUnit.MILLISECONDS.sleep(30)
      ()
    }
    val par = map2WithTimeouts(fork(unit(tooLong)), fork(unit(justRight)))((a, b) => { "Done" })(es)

    intercept[TimeoutException] {
      par.get(50, TimeUnit.MILLISECONDS)
    }
  }

  test("parFilter filters list elements") {
    parFilter((1 to 10).toList)(_ % 2 == 0)(es).get should be(List(2, 4, 6, 8, 10))
  }

  override def afterAll() {
    es.shutdown()
    es.awaitTermination(5, TimeUnit.SECONDS)
  }

}

