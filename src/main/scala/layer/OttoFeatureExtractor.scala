package layer

import layer.BaseExtractor.{ServiceInput, ServiceResult}
import zio.{Has, Task, ZLayer}

object OttoFeatureExtractor {

  trait Service {
    def additionalExtract(in: ServiceInput): Task[ServiceResult]
  }

  object OttoFeatureExtractorImpl extends Service {
    def additionalExtract(in: ServiceInput): Task[ServiceResult] = Task(in + 1)
  }

  val live: ZLayer[ServiceInput, Throwable, Has[ServiceResult]] =
    ZLayer.fromFunctionM(OttoFeatureExtractorImpl.additionalExtract)
}
