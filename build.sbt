name := "bootstrap"

version := "0.1"

sbtVersion := "0.13"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.14" % "test"
)

scalacOptions += "-feature"

scalacOptions += "-deprecation"

// Read here for optional dependencies:
// http://etorreborre.github.com/specs2/guide/org.specs2.guide.Runners.html#Dependencies
resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"

libraryDependencies ++= List(
  // use the right Slick version here:
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.h2database" % "h2" % "1.3.166"
)
