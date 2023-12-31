val scala3Version = "3.3.1"

val catsVersion = "2.10.0"
val catsEffectVersion = "3.5.2"
val fs2Version = "3.9.3"
val jenaVersion = "4.10.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "grokking-functional-programming",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "co.fs2" %% "fs2-core" % fs2Version,
      "org.apache.jena" % "apache-jena-libs" % jenaVersion,
      "org.apache.jena" % "jena-fuseki-main" % jenaVersion
    )
  )
