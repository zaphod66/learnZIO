package TicTacToe

// see: https://scalac.io/write-command-line-application-with-zio/
// see: https://github.com/ioleo/zio-by-example

import java.io.IOException

import zio.console._
import zio.{App, ExitCode, ZIO, console}
import zio.ExitCode._

object TicTacToeApp extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    program.foldM(
      error => console.putStrLn(s"Execution failed with: $error") *> ZIO.succeed(failure),
      _ => ZIO.succeed(success)
    )

  val program: ZIO[Console, IOException, Unit] =
    console.putStrLn("TicTacToe game!")

}
