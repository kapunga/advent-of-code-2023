val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent Of Code 2023",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "toolkit" % "0.1.19",
      "org.typelevel" %% "toolkit-test" % "0.1.19" % Test
    ),

    Compile / run / fork := true
  )
