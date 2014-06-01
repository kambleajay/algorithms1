name := "dijkstra"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.2"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.2"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

javaOptions in run += "-Xshare:off"