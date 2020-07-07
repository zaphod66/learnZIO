package layer

import zio.{ExitCode, URIO, ZIO}
import zio.console._
import zio.ExitCode._

object ZLayerApp extends zio.App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] = ???
}