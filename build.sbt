val zioVersion = "2.0.0-RC1"

libraryDependencies += "dev.zio" %% "zio" % zioVersion
libraryDependencies += "dev.zio" %% "zio-streams" % zioVersion
libraryDependencies += "dev.zio" %% "zio-test" % zioVersion % Test

scalaVersion := "2.13.6"

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))