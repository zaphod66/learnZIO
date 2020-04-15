package quickstart

// https://github.com/wi101/zio-examples/tree/master/src/main/scala/com/zio/examples/quickstart

import zio._
import zio.duration._
import zio.console.Console

case class Chocolate private (max: Int, tries: Ref[Int]) {
  val eat: ZIO[Console, String, Unit] = tries.modify { oldTries =>
    if (oldTries < max)
      (console.putStrLn("Eating \uD83C\uDF6B.... " + oldTries), oldTries + 1)   // \uD83D \uDC1D // \uD83C \uDF6B
    else
      (console.putStrLn("No chocolate left!") *> ZIO.fail("dummy"), oldTries)
  }.flatten
}

object Chocolate {
  def apply(max: Int): ZIO[Any, Nothing, Chocolate] = Ref.make(0).map(tries => new Chocolate(max, tries))
}

object QuickstartTest extends zio.App {

  def parseInt(s: String): Task[Int] = Task(s.toInt)

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {

    val io = for {
      _      <- console.putStrLn("Hello, how many chocolates would you like to eat?!")
      numStr <- console.getStrLn
      num    <- parseInt(numStr)
      choc   <- Chocolate(num)
      _      <- choc.eat.repeat(Schedule.spaced(250.millis) && Schedule.recurs(9)).option
    } yield num

    io.retry(Schedule.recurs(1)).foldM(
      _ => console.putStrLn(s"you don't like chocolate? ok.... BYE!") *> UIO(1),
      i => console.putStrLn(s"I hope you enjoyed :-) ZIO! ($i)") *> UIO(0)
    )
  }
}
