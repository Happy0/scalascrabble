import sbt._
import Keys._

trait Resolvers {
  val sonatype = "sonatype" at "http://oss.sonatype.org/content/repositories/releases"
}

trait Dependencies {
  // val scalaz = "org.scalaz" % "scalaz-core_2.10.0-RC2" % "6.0.4"
  val scalaz = "org.scalaz" %% "scalaz-core" % "6.0.4"
}

object ApplicationBuild extends Build with Resolvers with Dependencies {

  private val buildSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.10.1",
    version := "1",
    resolvers := Seq(sonatype),
    libraryDependencies := Seq(scalaz),
    scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-language:_")
  )

  lazy val main = Project("scalascrabble", file("."), settings = buildSettings)
}
