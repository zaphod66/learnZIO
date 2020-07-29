package layer

import layer.BaseExtractor.{ServiceInput, ServiceResult}
import zio.{Has, Task, ZLayer}

object SKPFeatureExtractor {

  trait Service {
    def additionalExtract(in: ServiceInput): Task[ServiceResult]
  }

  object SKPFeatureExtractorImpl extends Service {
    def additionalExtract(in: ServiceInput): Task[ServiceResult] = Task(in + 2)
  }

  val live: ZLayer[ServiceInput, Throwable, Has[ServiceResult]] =
    ZLayer.fromFunctionM(SKPFeatureExtractorImpl.additionalExtract)
}
