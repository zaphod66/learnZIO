package layer

// https://blog.softwaremill.com/managing-dependencies-using-zio-8acc1539e276

import zio.{ExitCode, ZIO, ZLayer}
import zio.console._
import zio.ExitCode._

object ZLayerApp extends zio.App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] = {

//    val featureExtractor = CoreFeatureExtractor.live ++ OttoFeatureExtractor.live
//
//    val otto = OttoFeatureExtractor.live
//    val program = ZIO.succeed(ExitCode.success)
//
//    val ll = program.provideLayer(featureExtractor).fold(_ => ExitCode.failure, _ => ExitCode.success)
//
//    program

    ZIO.succeed(ExitCode.success)
  }
}
