import Dependencies._

scalaVersion     := "2.13.1"
version          := "0.1.0-SNAPSHOT"
organization     := "com.zaphod"
organizationName := "learn"
name             := "learnZIO"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3")
addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")

libraryDependencies ++= logging ++ zio ++ pureConfig ++ Dependencies.test

testFrameworks ++= Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val root = project in file(".")
