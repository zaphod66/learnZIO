import layer.{CoreFeatureExtractor, OttoFeatureExtractor, SKPFeatureExtractor}
import zio.Has

package object FeatureExtractor {
  type CoreFeatureExtractor = Has[CoreFeatureExtractor.Service]
  type OttoFeatureExtractor = Has[OttoFeatureExtractor.Service]
  type SKPFeatureExtractor = Has[SKPFeatureExtractor.Service]
}
