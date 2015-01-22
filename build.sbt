name := "sisp"

version := "0.1"

sbtVersion := "0.13.7"

scalaVersion := "2.11.5"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-Yrangepos"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.1.3" % "test"
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
"""
