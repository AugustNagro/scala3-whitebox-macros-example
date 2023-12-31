val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-whitebox-macros-example",
    version := "0.1.0-SNAPSHOT",
//    scalacOptions += "-Xshow-phases",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
