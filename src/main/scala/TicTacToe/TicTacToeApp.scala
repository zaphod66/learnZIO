package TicTacToe

// see: https://scalac.io/write-command-line-application-with-zio/
// see: https://github.com/ioleo/zio-by-example

import java.io.IOException

import zio.console._
import zio.{App, ZIO, console}

object TicTacToeApp extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    program.foldM(
      error => console.putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )

  val program: ZIO[Console, IOException, Unit] =
    console.putStrLn("TicTacToe game!")

}
