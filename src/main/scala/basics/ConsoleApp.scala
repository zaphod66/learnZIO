package basics

import java.io.IOException

import zio.{App, ZIO}
import zio.console._

object ConsoleApp extends App {
  override def run(args: List[String]): ZIO[Console, Nothing, Int] = program.fold(
    _ => 1, // failure
    _ => 0  // success
  )

  private val program: ZIO[Console, IOException, Unit] = for {
    _    <- putStrLn("Hello, what is your name?")
//    name <- getStrLn
//    _    <- putStrLn(s"Hello $name!")
  } yield ()
}
