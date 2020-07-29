package layer

import layer.BaseExtractor.{ServiceInput, ServiceResult}
import zio.{Has, Task, ZLayer}

object CoreFeatureExtractor {
  trait Service {
    def extract(in: ServiceInput): Task[ServiceResult]
  }

  object CoreFeatureExtractorImpl extends Service {
    override def extract(in: ServiceInput): Task[ServiceResult] = Task(in + 42)
  }

  val live: ZLayer[ServiceInput, Throwable, Has[ServiceResult]] =
    ZLayer.fromFunctionM(CoreFeatureExtractorImpl.extract)
}
