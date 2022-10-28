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
