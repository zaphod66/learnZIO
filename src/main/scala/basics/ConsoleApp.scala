package basics

import zio._

object ConsoleApp extends App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = program

  private val logic = (for {
    _ <- console.putStrLn("I'm running")
  } yield 0)
    .catchAll(e => console.putStrLn(s"App failed $e").as(1))

  private val program = logic
}
