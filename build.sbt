import sbt.Keys.libraryDependencies

val zioVersion = "2.0.0-RC5"

Global / onChangedBuildSource := ReloadOnSourceChanges

libraryDependencies += "dev.zio"                      %% "zio"           % zioVersion
libraryDependencies += "dev.zio"                      %% "zio-macros"    % zioVersion
libraryDependencies += "dev.zio"                      %% "zio-streams"   % zioVersion
libraryDependencies += "dev.zio"                      %% "zio-parser"    % "0.1.7"
libraryDependencies += "dev.zio"                      %% "zio-json"      % "0.3.0-RC10"
libraryDependencies += "dev.zio"                      %% "zio-test"      % zioVersion % Test
libraryDependencies += "dev.zio"                      %% "zio-test-sbt"      % zioVersion % Test
libraryDependencies += "io.d11"                       %% "zhttp"         % "2.0.0-RC7"
libraryDependencies += "com.softwaremill.magnolia1_2" %% "magnolia"      % "1.1.2"
libraryDependencies += "org.scala-lang"                % "scala-reflect" % scalaVersion.value

scalaVersion := "2.13.8"
scalacOptions ++= Seq("-Ymacro-annotations")

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

reStart / mainClass := Some("api.Backend")

val coreDemosZioVersion = "2.0.0"
lazy val coreDemos = (project in (file("coreDemos")))
  .settings(
    libraryDependencies += "dev.zio"                      %% "zio"           % coreDemosZioVersion,
    libraryDependencies += "dev.zio"                      %% "zio-test"      % coreDemosZioVersion % Test,
    libraryDependencies += "dev.zio"                      %% "zio-test-sbt"      % coreDemosZioVersion % Test,
    libraryDependencies += "dev.zio"                      %% "zio-logging"           % coreDemosZioVersion,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )