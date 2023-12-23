val scala3Version = "3.3.1"

val catsVersion = "2.10.0"
val catsEffectVersion = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "grokking-functional-programming",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    )
  )
