package fpinscala.parallelism

import org.scalatest.{ BeforeAndAfterAll, FunSuite }
import org.scalatest.Matchers._
import java.util.concurrent._
import Par._

class Chapter7Suite extends FunSuite with BeforeAndAfterAll {

  private val es = Executors.newCachedThreadPool()

  test("parFilter filters list elements") {
    parFilter((1 to 10).toList)(_ % 2 == 0)(es).get should be(List(2, 4, 6, 8, 10))
  }

  override def afterAll() {
    es.shutdown()
    es.awaitTermination(5, TimeUnit.SECONDS)
  }

}

