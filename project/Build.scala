import sbt._
import Keys._

trait Resolvers {
  val sonatype = "releases" at "http://oss.sonatype.org/content/repositories/releases"
  val snapshots = "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
}

trait Dependencies {
  // val scalaz = "org.scalaz" % "scalaz-core_2.10.0-RC2" % "6.0.4"
  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.4"
  val specs2 = "org.specs2" %% "specs2" % "1.14" % "test"

}

object ApplicationBuild extends Build with Resolvers with Dependencies {

  private val buildSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.10.1",
    version := "1",
    resolvers := Seq(sonatype, snapshots),
    libraryDependencies := Seq(scalaz, specs2),
    scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-language:_"))

  lazy val main = Project("scalascrabble", file("."), settings = buildSettings)
}
