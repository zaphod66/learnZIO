// Itamar Ravid - A Tour of ZIO Streams
// https://www.youtube.com/watch?v=OiuKbpMOKsc

package streams

import zio.{App, ExitCode, UIO, URIO, ZIO, random}
import zio.console.{getStrLn, putStrLn}
import zio.stream._

import java.lang.System.console
import java.nio.file.Paths

object Types {
  val oneSuccess: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  val oneFailure: ZIO[Any, Throwable, Int] = ZIO.fail(new Throwable)

  val requiresRandom: ZIO[random.Random, Nothing, Int] = random.nextInt

  val threeVals: ZStream[Any, Nothing, Int] = ZStream(1, 2, 3)
  val emptyStream: ZStream[Any, Nothing, Nothing] = ZStream.empty
  val valThenFail: ZStream[Any, Throwable, Int] = ZStream(1, 2) ++ ZStream.fail(new Throwable)
}

object HelloWorld extends App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    for {
      elements <- ZStream("Hello", "World").runCollect
      _        <- putStrLn(elements.toString)
    } yield ExitCode.success
}

object InfiniteStream extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZStream.iterate(0)(_ + 1).take(20).runCollect.flatMap { chunk =>
      putStrLn(chunk.toString)
    }.exitCode
}

object Effect extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    (ZStream.fromEffect(putStrLn("Hello")).drain ++
//      ZStream.iterate(0)(_ + 1)).mapM(i => putStrLn((i * 2).toString).as(i)).take(20) ++
      ZStream.iterate(0)(_ + 1)).tap(i => putStrLn((i * 2).toString)).take(20).runCollect.exitCode
}

object ControlFlow extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZStream.repeatEffect(getStrLn).take(5).tap(line => putStrLn(line) *> putStrLn(line)).runCollect.exitCode
}

object Transforming extends App {
  case class StockQuote(sym: String, openPrice: Double, closingPrice: Double)

  val streamStocks = ZStream(StockQuote("DDOG", 37.04, 39.00), StockQuote("NET", 18.04, 19.01))
  val streamSymbol = streamStocks.map(_.sym)
  val streamOpenAndClose = streamStocks.flatMap {
    case StockQuote(sym, openPrice, closingPrice) =>
      ZStream(sym -> openPrice, sym -> closingPrice)
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    streamOpenAndClose.runCollect.flatMap { chunk =>
      putStrLn(chunk.toString)
    }.exitCode
}

object Transducing extends App {
  val filePath = "/Users/nscheller/tmp/Stocks.csv"
  val stream   = ZStream.fromFile(Paths.get(filePath))
    .transduce(ZTransducer.utf8Decode >>> ZTransducer.splitLines)
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ???

}