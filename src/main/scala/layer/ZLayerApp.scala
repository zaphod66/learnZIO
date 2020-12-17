package layer

import zio._
import zio.console.Console

object ZLayerApp extends zio.App {

  type MeaningOfLive = Has[MeaningOfLive.Service]

  val meaningOfLive = ZIO.environment[Has[Int] with Console].flatMap(env => console.putStrLn(s"The meaning of Live is ${env.get}."))
  object MeaningOfLive {
    trait Service
  }

  val consoleLayer = zio.console.Console.live
  val intLayer = ZLayer.fromManaged {
    ZManaged.make(UIO(println("Acquiring Int")).as(42)){ _ => UIO.effectTotal(println("Releasing Int"))}
  }

  val combinedLayer = intLayer ++ consoleLayer


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    meaningOfLive.provideLayer(combinedLayer).exitCode
}
