import Dependencies._

val scala3Version = "3.3.0"

lazy val modules = file("modules")

lazy val defaultSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := scala3Version,
  libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
)

lazy val kernel =
  project.in(modules / "kernel")
    .settings(
      defaultSettings,
      name := "scalka-kernel"
    )

lazy val cats =
  project.in(modules / "cats")
    .settings(
      defaultSettings,
      name := "scalka-cats",
      libraryDependencies += catsCore
    ).dependsOn(kernel)

lazy val root = project
  .in(file("."))
  .settings(
    defaultSettings,
    name := "scalka"
  ).aggregate(kernel, cats)
