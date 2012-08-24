import sbt._
import sbt.Project.Initialize
import sbt.Keys._

object BigtopBuild extends Build {

  val bigtopVersion = "0.1"

  lazy val bigtopResolvers = Seq(
    "Sonatype"      at "http://oss.sonatype.org/content/repositories/releases",
    "Sonatype Snap" at "http://oss.sonatype.org/content/repositories/snapshots",
    "JBoss"         at "http://repository.jboss.org/nexus/content/groups/public",
    "Typesafe"      at "http://repo.typesafe.com/typesafe/releases",
    "ReportGrid"    at "http://nexus.reportgrid.com/content/repositories/public-snapshots",
    "Untyped"       at "http://repo.untyped.com/",
    "Twitter"       at "http://maven.twttr.com"
  )

  lazy val scalaz        = "org.scalaz"              %% "scalaz-core"     % "7.0-SNAPSHOT" changing()
  lazy val blueeyesCore  = "com.github.jdegoes"      %% "blueeyes-core"   % "0.6.0-SNAPSHOT" changing()
  lazy val blueeyesMongo = "com.github.jdegoes"      %% "blueeyes-mongo"  % "0.6.0-SNAPSHOT" changing()
  lazy val blueeyesJson  = "com.github.jdegoes"      %% "blueeyes-json"   % "0.6.0-SNAPSHOT" changing()
  lazy val specs2        = "org.specs2"              %% "specs2"          % "1.8.1"
  lazy val scalacheck    = "org.scala-tools.testing" %% "scalacheck"      % "1.9"
  lazy val jbCrypt       = "org.mindrot"             %  "jbcrypt"         % "0.3m"
  lazy val redisclient   = "net.debasishg"           %% "redisclient"     % "2.4.2"
  lazy val twitterUtil   = "com.twitter"             %% "util-collection" % "1.12.12"
  lazy val jodaTime      = "joda-time"               %  "joda-time"       % "2.0"
  lazy val jodaConvert   = "org.joda"                %  "joda-convert"    % "1.2"
  lazy val slf4s         = "com.weiglewilczek.slf4s" %% "slf4s"           % "1.0.7"
  lazy val configrity    = "org.streum"              %% "configrity"      % "0.9.0"
  lazy val metricsCore   = "com.yammer.metrics"      %  "metrics-core"    % "2.1.2"
  lazy val metricsScala  = "com.yammer.metrics"      %% "metrics-scala"   % "2.1.2"

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

  val publishSettings = Seq(
    organization := "com.untyped",
    scalaVersion := "2.9.1",
    version := bigtopVersion,
    publishMavenStyle := false,
    publishTo <<= (version) { version: String =>
      for {
        host    <- Option(System.getenv("DEFAULT_IVY_REPO_HOST"))
        path    <- Option(System.getenv("DEFAULT_IVY_REPO_PATH"))
        user    <- Option(System.getenv("DEFAULT_IVY_REPO_USER"))
        keyfile <- Option(System.getenv("DEFAULT_IVY_REPO_KEYFILE"))
      } yield Resolver.sftp("UntypedPublish", host, path)(Resolver.ivyStylePatterns).as(user, file(keyfile))
    }
  )

  lazy val root = Project(
    id = "root",
    base = file(".")
  ).settings(
    publishSettings : _*
  ).aggregate(
    core,
    user,
    util
  )

  lazy val core = Project(
    id = "bigtop-core",
    base = file("core")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    publishSettings ++
    Seq(
      libraryDependencies ++= Seq(specs2, jbCrypt, jodaTime, jodaConvert)
    ) : _*
  )

  lazy val user = Project(
    id = "bigtop-user",
    base = file("user")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    publishSettings ++
    Seq(
      libraryDependencies += twitterUtil
    ) : _*
  ).dependsOn(core)

  lazy val util = Project(
    id = "bigtop-util",
    base = file("util")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    publishSettings ++
    Seq(
      libraryDependencies ++= Seq(specs2, metricsCore, metricsScala)
    ) : _*
  ).dependsOn(core)

  lazy val redis = Project(
    id = "bigtop-redis",
    base = file("redis")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    publishSettings ++
    Seq(
      libraryDependencies += redisclient
    ) : _*
  ).dependsOn(core)

}
