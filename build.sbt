val catsVersion = "2.0.0"

lazy val cronparser = (project in file(".")).settings(
  name := "cron-parser",
  version := "0.1",
  scalaVersion := "2.13.1",
  libraryDependencies ++= Seq(

    "org.typelevel" %% "cats-core"   % catsVersion,
    "org.typelevel" %% "cats-macros" % catsVersion,
    "org.typelevel" %% "cats-kernel" % catsVersion,
    "org.typelevel" %% "cats-effect" % "2.1.3" withSources() withJavadoc(),
    "org.scalactic" %% "scalactic"   % "3.0.8" % "test",
    "org.scalatest" %% "scalatest"   % "3.0.8" % "test",
    "org.scalatestplus" %% "scalatestplus-scalacheck"   % "3.1.0.0-RC2" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
  ),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)

scapegoatVersion in ThisBuild := "1.4.3"
