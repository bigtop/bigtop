import sbt._
import sbt.Project.Initialize
import sbt.Keys._

object BigtopBuild extends Build {

  val bigtopVersion = "0.1"

  lazy val bigtopResolvers = Seq(
    "Sonatype"    at "http://oss.sonatype.org/content/repositories/releases",
    "Scala Tools" at "http://scala-tools.org/repo-snapshots",
    "JBoss"       at "http://repository.jboss.org/nexus/content/groups/public",
    "Akka"        at "http://akka.io/repository"
  )

  lazy val scalaz      = "org.scalaz"              %% "scalaz-core" % "6.0.3"
  lazy val blueeyes    = "com.reportgrid"          %% "blueeyes"    % "0.4.24"
  lazy val specs       = "org.scala-tools.testing" %% "specs"       % "1.6.9"
  lazy val scalacheck  = "org.scala-tools.testing" %% "scalacheck"  % "1.9"
  lazy val configgy    = "net.lag"                 %  "configgy"    % "2.0.0"
  lazy val jbCrypt     = "org.mindrot"             %  "jbcrypt"     % "0.3m"
  lazy val redisclient = "net.debasishg"           %% "redisclient" % "2.4.2"

  val blueeyesSettings = Seq(
    resolvers := bigtopResolvers,
    libraryDependencies ++= Seq(
      blueeyes,
      specs      % "test",
      scalacheck % "test",
      configgy   % "compile" intransitive(),
      jbCrypt
    ),
    exportJars := true,
    ivyXML :=
      <dependencies>
        <dependency org="com.reportgrid" name="blueeyes_2.9.1" rev="0.4.24">
          <exclude module="configgy"/>
        </dependency>
      </dependencies>
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
      version := bigtopVersion
    ) : _*
  ).dependsOn(core)

  lazy val session = Project(
    id = "session",
    base = file("session")
  ).settings(
    Project.defaultSettings ++
    blueeyesSettings ++
    Seq(
      version := bigtopVersion
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
      version := bigtopVersion
    ) : _*
  )

}
