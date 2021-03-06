name := "sisp"

version := "0.1"

sbtVersion := "0.13.12"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-Yrangepos"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "jline" % "jline" % "2.12"
)

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots"
)


initialCommands += """
"""

initialCommands in console += """
import scala.language._
import scala.language.implicitConversions._
import com.daewon.sisp._
import com.daewon.sisp.Sisp._
"""
