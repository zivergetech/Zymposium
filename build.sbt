Global / onChangedBuildSource := ReloadOnSourceChanges
val zioVersion = "2.0.2"

val sharedSettings =
  Seq(
    scalaVersion := "2.13.8"
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
