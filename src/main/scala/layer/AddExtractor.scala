package layer

import layer.BaseExtractor.{ServiceInput, ServiceResult}
import zio.Task

trait AddExtractor {
  def additionalExtract(in: ServiceInput): Task[ServiceResult]
}
