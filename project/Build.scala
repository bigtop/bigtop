import sbt._
import sbt.Project.Initialize
import sbt.Keys._

object BigtopBuild extends Build {

  val bigtopVersion = "0.1"

  lazy val bigtopResolvers = Seq(
    "Sonatype"    at "http://oss.sonatype.org/content/repositories/releases",
    "Scala Tools" at "http://scala-tools.org/repo-snapshots",
    "JBoss"       at "http://repository.jboss.org/nexus/content/groups/public",
    "Typesafe"    at "http://repo.typesafe.com/typesafe/releases",
    "ReportGrid"  at "http://nexus.reportgrid.com/content/repositories/public-snapshots",
    "Untyped"     at "http://repo.untyped.com/",
    "Twitter"     at "http://maven.twttr.com"
  )

  lazy val scalaz        = "org.scalaz"              %% "scalaz-core"     % "7.0-SNAPSHOT"
  lazy val blueeyesCore  = "com.reportgrid"          %% "blueeyes-core"   % "0.6.0-UNTYPED"
  lazy val blueeyesMongo = "com.reportgrid"          %% "blueeyes-mongo"  % "0.6.0-UNTYPED"
  lazy val blueeyesJson  = "com.reportgrid"          %% "blueeyes-json"   % "0.6.0-UNTYPED"
  lazy val specs2        = "org.specs2"              %% "specs2"          % "1.8.1"
  lazy val scalacheck    = "org.scala-tools.testing" %% "scalacheck"      % "1.9"
  lazy val jbCrypt       = "org.mindrot"             %  "jbcrypt"         % "0.3m"
  lazy val redisclient   = "net.debasishg"           %% "redisclient"     % "2.4.2"
  lazy val twitterUtil   = "com.twitter"             %% "util-collection" % "1.12.12"
  lazy val jodaTime      = "joda-time"               %  "joda-time"       % "2.0"
  lazy val jodaConvert   = "org.joda"                %  "joda-convert"    % "1.2"
  lazy val slf4s         = "com.weiglewilczek.slf4s" %% "slf4s"           % "1.0.7"
  lazy val configrity    = "org.streum"              %% "configrity"      % "0.9.0"

  val blueeyesSettings = Seq(
    resolvers := bigtopResolvers,
    libraryDependencies ++= Seq(
      blueeyesCore,
      blueeyesMongo,
      blueeyesJson,
      specs2     % "test",
      scalacheck % "test",
      jbCrypt,
      slf4s,
      configrity
    ),
    exportJars := true
  )

  lazy val root = Project(
    id = "root",
    base = file(".")
  ).aggregate(core, user)

  lazy val user = Project(
    id = "user",
    base = file("user")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    Seq(
      version := bigtopVersion,
      libraryDependencies += twitterUtil
    ) : _*
  ).dependsOn(core)

  lazy val redis = Project(
    id = "redis",
    base = file("redis")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    Seq(
      version := bigtopVersion,
      libraryDependencies += redisclient
    ) : _*
  ).dependsOn(core)

  lazy val core = Project(
    id = "core",
    base = file("core")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    Seq(
      version := bigtopVersion,
      libraryDependencies ++= Seq(specs2, jbCrypt, jodaTime, jodaConvert)
    ) : _*
  )

}
