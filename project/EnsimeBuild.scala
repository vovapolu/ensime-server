import java.io._
import scala.util.{ Properties, Try }

import com.lucidchart.sbt.scalafmt.ScalafmtCorePlugin.autoImport._
import de.heikoseeberger.sbtheader.{ HeaderKey, HeaderPlugin }
import sbt.Keys._
import sbt.{ IntegrationTest => It, _ }
import sbtassembly.AssemblyKeys._
import sbtassembly.{ AssemblyKeys, MergeStrategy, PathList }
import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._

import org.ensime.EnsimePlugin.JdkDir
import org.ensime.EnsimeKeys._

import fommil.SensibleSettings._
import fommil.SonatypeKeys._

object ProjectPlugin extends AutoPlugin {
  override def requires = fommil.SensiblePlugin
  override def trigger = allRequirements

  override def buildSettings = Seq(
    scalaVersion := "2.12.3",
    organization := "org.ensime",

    // so M2 releases don't impact SNAPSHOT versioning
    version := version.value.replaceAll("(-M.*|-RC.*)-SNAPSHOT", "-SNAPSHOT"),

    ensimeIgnoreMissingDirectories := true,
    ensimeJavaFlags += "-Xmx4g",

    scalafmtConfig in ThisBuild := file("project/scalafmt.conf"),
    scalafmtVersion in ThisBuild := "1.3.0",

    sonatypeGithub := ("ensime", "ensime-server"),
    licenses := Seq(GPL3),
    startYear := Some(2010)
  ) ++ addCommandAlias(
    "fmt",
    ";testutil/createHeaders ;createHeaders ;test:createHeaders ;it:createHeaders ;testutil/scalafmt ;testutil/test:scalafmt ;scalafmt ;test:scalafmt ;it:scalafmt ;sbt:scalafmt"
  )

  override def projectSettings = Seq(
    scalacOptions in Compile -= "-Ywarn-value-discard",
    scalacOptions ++= Seq("-language:postfixOps", "-language:implicitConversions"),
    scalafmtOnCompile := true
  )
}

object EnsimeBuild {
  // common to the ensimeBuild, but not the testing projects
  lazy val commonSettings = Seq(
    dependencyOverrides ++= Set(
       "com.typesafe.akka" %% "akka-actor" % akkaVersion,
       "com.typesafe.akka" %% "akka-testkit" % akkaVersion
    ),

    // disabling shared memory gives a small performance boost to
    // tests but jvisualvm will no longer see the process.
    javaOptions += "-XX:+PerfDisableSharedMem",
    javaOptions ++= Seq("-Xms512m", "-Xmx512m"),

    // only recognised by 2.12.2+
    javaOptions += "-Dscala.classpath.closeZip=true",

    // print the table to optimise your own apps. VFS (and OrientDB)
    // are heavy on interning.
    javaOptions ++= Seq(
      //"-XX:+PrintStringTableStatistics",
      "-XX:StringTableSize=1000003",
      "-XX:+UnlockExperimentalVMOptions",
      "-XX:SymbolTableSize=1000003"
    ),

    dependencyOverrides ++= Set(
      "org.apache.lucene" % "lucene-core" % luceneVersion
    ),

    updateOptions := updateOptions.value.withCachedResolution(true)
  )

  lazy val commonItSettings = inConfig(It)(
    Defaults.testSettings ++ sensibleTestSettings ++ scalafmtSettings ++ Seq(
      // speeds up the tests a bit without breaking appveyor / travis limits
      javaOptions ++= Seq("-Xms1400m", "-Xmx1400m")
    )
  ) ++ HeaderPlugin.settingsFor(It)

  lazy val JavaTools: File = JdkDir / "lib/tools.jar"

  ////////////////////////////////////////////////
  // modules
  lazy val monkeys = Project("monkeys", file("monkeys")) settings (commonSettings) settings (
    // WORKAROUND https://issues.scala-lang.org/browse/SI-10157
    scalacOptions in (Compile, doc) -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.apache.commons" % "commons-vfs2" % "2.1" exclude ("commons-logging", "commons-logging")
    )
  )

  lazy val api = Project("api", file("api")) settings (commonSettings) settings (
    licenses := Seq(Apache2)
  )

  lazy val util = Project("util", file("util")) settings (commonSettings) dependsOn (
    api
  ) settings (
    libraryDependencies ++= List(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.apache.commons" % "commons-vfs2" % "2.1" exclude ("commons-logging", "commons-logging"),
      "com.google.code.findbugs" % "jsr305" % "3.0.2" % "provided"
    ) ++ logback ++ shapeless.value
  )

  lazy val testutil = Project("testutil", file("testutil")) settings (commonSettings) dependsOn (
    util, api
  ) settings (
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
      ) ++sensibleTestLibs(Compile)
    )

  lazy val json = project settings(commonSettings) settings (
    licenses := Seq(LGPL3),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
    ) ++ shapeless.value
  )

  lazy val s_express = Project("s-express", file("s-express")) settings (commonSettings) settings (
      licenses := Seq(LGPL3),
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "fastparse" % "0.4.4",
        "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
      ) ++ shapeless.value ++ logback
    )

  // the JSON protocol
  lazy val jerky = Project("jerky", file("protocol-jerky")) settings (commonSettings) dependsOn (
    util,
    json,
    api,
    testutil % Test,
    api % "test->test" // for the test data
  ) settings (
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
      ) ++ shapeless.value
    )

  // the S-Exp protocol
  lazy val swanky = Project("swanky", file("protocol-swanky")) settings (commonSettings) dependsOn (
    api, s_express, util,
    testutil % Test,
    api % "test->test" // for the test data
  ) settings (
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
      ) ++ shapeless.value
    )

  import EnsimeTestingBuild._
  lazy val core = Project("core", file("core")).dependsOn(
    api, s_express, monkeys, util,
    api % "test->test", // for the interpolator
    testutil % "test,it",
    // depend on "it" dependencies in Test or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    testingEmpty % "test,it",
    testingSimple % "test,it",
    // test config needed to get the test jar
    testingSimpleJar % "test,it->test",
    testingTiming % "test,it",
    testingMacros % "test, it",
    testingShapeless % "test,it",
    testingDebug % "test,it",
    testingJava % "test,it"
  ).configs(It).settings(
      commonSettings, commonItSettings
    ).settings(
      unmanagedJars in Compile += JavaTools,
      ensimeUnmanagedSourceArchives += (baseDirectory in ThisBuild).value / "openjdk-langtools/openjdk8-langtools-src.zip",
      libraryDependencies ++= Seq(
        "com.orientechnologies" % "orientdb-graphdb" % orientVersion
          exclude ("commons-collections", "commons-collections")
          exclude ("commons-beanutils", "commons-beanutils")
          exclude ("commons-logging", "commons-logging"),
        "org.apache.lucene" % "lucene-core" % luceneVersion,
        "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
        "org.ow2.asm" % "asm-commons" % "5.2",
        "org.ow2.asm" % "asm-util" % "5.2",
        "org.scala-lang" % "scalap" % scalaVersion.value,
        "com.typesafe.akka" %% "akka-actor" % akkaVersion,
        "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
        {
          // see notes in https://github.com/ensime/ensime-server/pull/1446
          val suffix = CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((2, 11)) => "2.11.8"
            case _             => "2.12.2"
          }
          "org.scala-refactoring" % s"org.scala-refactoring.library_${suffix}" % "0.13.0"
        },
        "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
        "org.scala-debugger" %% "scala-debugger-api" % "1.1.0-M3"
      ) ++ shapeless.value
    ) enablePlugins BuildInfoPlugin settings (
        buildInfoPackage := organization.value,
        buildInfoKeys += BuildInfoKey.action("gitSha")(Try("git rev-parse --verify HEAD".!! dropRight 1) getOrElse "n/a"),
        buildInfoKeys += BuildInfoKey.action("builtAtString")(currentDateString())
      )

  private def currentDateString() = {
    val dtf = new java.text.SimpleDateFormat("yyyy-MM-dd")
    dtf.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    dtf.format(new java.util.Date())
  }

  val luceneVersion = "6.4.2" // 6.6 deprecates index time boosting
  val nettyVersion = "4.1.15.Final"
  lazy val server = Project("server", file("server")).dependsOn(
    core, swanky, jerky,
    s_express % "test->test",
    swanky % "test->test",
    // depend on "it" dependencies in Test or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    core % "test->test",
    core % "it->it",
    testingDocs % "test,it"
  ).configs(It).settings(
      commonSettings ++ commonItSettings
    ).settings(
        libraryDependencies ++= Seq(
          "io.netty"    %  "netty-transport"  % nettyVersion,
          "io.netty"    %  "netty-handler"    % nettyVersion,
          "io.netty"    %  "netty-codec-http" % nettyVersion
        ) ++ shapeless.value
      )

  lazy val lsp = Project("lsp", file("lsp")).dependsOn(
    core,
    core % "test->test",
    core % "it->it",
    json
  )

  // manual root project so we can exclude the testing projects from publication
  lazy val root = Project(id = "ensime", base = file(".")) settings (commonSettings) aggregate (
    api, monkeys, util, s_express, jerky, swanky, core, server, json, lsp
  ) dependsOn (server) settings (
      // e.g. `sbt ++2.11.11 ensime/assembly`
      test in assembly := {},
      sourceDirectories in Compile := Nil,
      resources in Compile := Nil,
      aggregate in assembly := false,
      assemblyMergeStrategy in assembly := {
        case PathList("org", "apache", "commons", "vfs2", xs @ _*) => MergeStrategy.first // assumes our classpath is setup correctly
        case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.concat // assumes our classpath is setup correctly
        case PathList("LICENSE") => MergeStrategy.concat // WORKAROUND https://github.com/sbt/sbt-assembly/issues/224
        case PathList("LICENSE.apache2") => MergeStrategy.first // WORKAROUND https://github.com/sbt/sbt-assembly/issues/224
        case PathList("NOTICE") => MergeStrategy.concat // WORKAROUND https://github.com/sbt/sbt-assembly/issues/224
        case other => MergeStrategy.defaultMergeStrategy(other)
      },
      assemblyExcludedJars in assembly := {
        val everything = (fullClasspath in assembly).value
        everything.filter { attr =>
          val n = attr.data.getName
          n.startsWith("scala-library") | n.startsWith("scala-compiler") |
            n.startsWith("scala-reflect") | n.startsWith("scalap")
        } :+ Attributed.blank(JavaTools)
      },
      assemblyJarName in assembly := s"ensime_${scalaBinaryVersion.value}-${version.value}-assembly.jar"
    )

  private val akkaVersion = "2.5.4"
  private val orientVersion = "2.2.27"
}

// projects used in the integration tests, not published
object EnsimeTestingBuild {
  private def testingProject(dir: String) = Project(dir.replace("/", "_"), file(dir)).settings(
    scalacOptions in (Compile, compile) := Nil,
    scalacOptions in (Test, compile) := Nil,
    libraryDependencies := Seq("org.scala-lang" % "scala-library" % scalaVersion.value),
    scalafmtOnCompile := false
  )

  lazy val testingEmpty = testingProject("testing/empty")

  lazy val testingSimple = testingProject("testing/simple") settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test intransitive ()
  )

  lazy val testingSimpleJar = testingProject("testing/simpleJar").settings(
    exportJars := true,
    ensimeUseTarget in Compile := Some((artifactPath in (Compile, packageBin)).value),
    ensimeUseTarget in Test := Some((artifactPath in (Test, packageBin)).value)
  )

  lazy val testingImplicits = testingProject("testing/implicits")

  lazy val testingTiming = testingProject("testing/timing")

  lazy val testingMacros = testingProject("testing/macros") settings (
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

  // just to have access to shapeless
  lazy val testingShapeless = testingProject("testing/shapeless").settings (
    libraryDependencies ++= shapeless.value
  )

  lazy val testingFqns = testingProject("testing/fqns").settings (
    libraryDependencies ++= shapeless.value ++ Seq(
      "org.typelevel" %% "cats" % "0.8.1" % Test intransitive(),
      "org.spire-math" %% "spire" % "0.13.0" % Test intransitive()
    )
  )

  lazy val testingDebug = testingProject("testing/debug")

  lazy val testingDocs = testingProject("testing/docs").settings(
    dependencyOverrides ++= Set("com.google.guava" % "guava" % "18.0"),
    libraryDependencies ++= Seq(
      "com.github.dvdme" % "ForecastIOLib" % "1.5.1" intransitive (),
      "com.google.guava" % "guava" % "18.0"
    )
  )

  // java project with no scala-library
  lazy val testingJava = testingProject("testing/java").settings(
    crossPaths := false,
    autoScalaLibrary := false,
    libraryDependencies := Nil
  )
}
