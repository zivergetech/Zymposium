Global / onChangedBuildSource := ReloadOnSourceChanges

val zioVersion = "2.0.8"

val sharedSettings =
  Seq(
    scalaVersion := "3.2.0"
  )

lazy val root = (project in file("."))
  .settings(
    name := "zymposium"
  )

lazy val zioFromScratch = (project in file("sessions/zio-from-scratch"))
  .settings(
    name := "zio-from-scratch",
    libraryDependencies ++= Seq()
  )
  .settings(sharedSettings: _*)

lazy val shardCakeDemo = (project in file("sessions/shardcake-demo"))
  .settings(
    name := "shardcake-demo",
    libraryDependencies ++= Seq(
      "dev.zio"        %% "zio"                     % "2.0.3",
      "com.devsisters" %% "shardcake-manager"       % "2.0.4",
      "com.devsisters" %% "shardcake-protocol-grpc" % "2.0.4"
    )
  )
  .settings(sharedSettings: _*)

lazy val zioDirectDemo = (project in file("sessions/zio-direct-demo"))
  .settings(
    name := "zio-direct-demo",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.5",
      "dev.zio" %% "zio-direct" % "1.0.0-RC1"
    )
  )
  .settings(sharedSettings: _*)

lazy val webCrawler = (project in file("sessions/web-crawler"))
  .settings(
    name := "zio-direct-demo",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
    )
  )
  .settings(sharedSettings: _*)

lazy val zioOpenAIDemo = (project in file("sessions/zio-openai-demo"))
  .settings(
    name := "zio-direct-demo",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-openai" % "0.1.0",
    )
  )
  .settings(sharedSettings: _*)
