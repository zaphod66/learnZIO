package basics

import zio.test._
import zio.test.Assertion._

object ConsoleAppTest extends DefaultRunnableSpec(
  suite("ConsoleApp") {
    testM("should complete successfully") {
      for {
        result <- ConsoleApp.run(List.empty[String])
      } yield assert(result, equalTo(0))
    }
  }
)
