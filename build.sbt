scalaVersion := "2.11.5"

name := "fsm"

version := "0.1"

lazy val root = (project in file(".")).aggregate(macros, examples)

lazy val macros = project.in(file("modules/macros"))

lazy val examples = project.in(file("modules/examples")).dependsOn(macros)
