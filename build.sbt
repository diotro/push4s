name := "push4s"

version := "0.1"

scalaVersion := "2.13.0"


libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.7"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.6.7"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

resolvers += Resolver.sonatypeRepo("releases")
libraryDependencies += "io.evvo" %% "evvo" % "0.0.0"
