import sbt._

object Dependencies {
  private val logbackVersion    = "1.2.3"
  private val loggingVersion    = "3.9.2"
  private val zioVersion        = "1.0.0-RC18-2"
  private val pureConfigVersion = "0.12.1"

  lazy val logging = List(
    "ch.qos.logback"             % "logback-classic" % logbackVersion,
    "com.typesafe.scala-logging" %% "scala-logging"  % loggingVersion
  )
  lazy val zio = List(
    "dev.zio" %% "zio" % zioVersion
  )
  lazy val pureConfig = List(
    "com.github.pureconfig" %% "pureconfig"      % pureConfigVersion,
    "com.github.pureconfig" %% "pureconfig-yaml" % pureConfigVersion
  )
  lazy val test = List(
    "dev.zio" %% "zio-test"     % zioVersion,
    "dev.zio" %% "zio-test-sbt" % zioVersion
  ).map(_ % Test)

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
}
