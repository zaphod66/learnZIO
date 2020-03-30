package TicTacToe

import zio.{console, App , ZEnv, ZIO}
import zio.console.Console

class TicTacToeApp extends App {

  val program: ZIO[Console, Nothing, Unit] = console.putStrLn("TicTacToe game!")

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    program.fold(
      error => console.putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}
