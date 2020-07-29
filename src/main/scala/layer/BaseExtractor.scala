package layer

import zio.Task

object BaseExtractor {
  type ServiceInput  = Int
  type ServiceResult = Int
}

trait BaseExtractor {
  import BaseExtractor._

  def extract(in: ServiceInput): Task[ServiceResult]
}
