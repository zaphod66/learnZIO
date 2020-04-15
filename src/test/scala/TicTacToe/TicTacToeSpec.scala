package TicTacToe

import zio.ZIO
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

class TicTacToeSpec {

}

object TicTacToeSpec extends DefaultRunnableSpec(

  suite("TicTacToe") (
    testM("prints to console") {
      for {
        out <- ZIO.collectAll(Iterable.single(TicTacToeApp.program))
      } yield ()
    }
  )
)