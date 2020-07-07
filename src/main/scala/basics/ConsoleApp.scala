package basics

import java.io.IOException

import zio.{App, ExitCode, ZIO}
import zio.console._
import zio.ExitCode._

object ConsoleApp extends App {
  override def run(args: List[String]): ZIO[Console, Nothing, ExitCode] = program.fold(
    _ => failure,
    _ => success
  )

  private val program: ZIO[Console, IOException, Unit] = for {
    _    <- putStrLn("Hello, what is your name?")
    name <- getStrLn
    _    <- putStrLn(s"Hello $name!")
  } yield ()
}
