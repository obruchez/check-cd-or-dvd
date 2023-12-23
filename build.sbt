name := "check-cd-or-dvd"

version := "1.1"

scalaVersion := "2.13.12"

ThisBuild / scalafmtOnCompile := true

assembly / mainClass := Some("org.bruchez.olivier.checkcdordvd.CheckCdOrDvd")
assembly / assemblyJarName := "check-cd-or-dvd.jar"
