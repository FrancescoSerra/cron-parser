val catsVersion = "2.0.0"

lazy val cronparser = (project in file(".")).settings(
  name := "cron-parser",
  version := "0.1",
  scalaVersion := "2.13.1",
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic"   % "3.0.8" % "test",
    "org.scalatest" %% "scalatest"   % "3.0.8" % "test",
    "org.typelevel" %% "cats-core"   % catsVersion,
    "org.typelevel" %% "cats-macros" % catsVersion,
    "org.typelevel" %% "cats-kernel" % catsVersion
  ),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)


