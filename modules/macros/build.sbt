scalaVersion := "2.11.5"

name := "macros"

version := "0.1"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.5"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
