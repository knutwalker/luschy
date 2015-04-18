import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import de.heikoseeberger.sbtheader.license.Apache2_0
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.ReleaseStep
import xerial.sbt.Sonatype.SonatypeKeys.sonatypeReleaseAll

lazy val parent = project in file(".") dependsOn core aggregate (core, tests) settings luschySettings

lazy val core = project enablePlugins AutomateHeaderPlugin settings (
  luschySettings,
  publishThis,
  name := "luschy",
  libraryDependencies ++= List(
    "org.apache.lucene" % "lucene-core" % luceneVersion.value,
    "com.chuusai"      %% "shapeless"   % shapelessVersion.value))

lazy val tests = project enablePlugins AutomateHeaderPlugin dependsOn core settings (
  luschySettings,
  scalacOptions in Test += "-Yrangepos",
  name := "luschy-tests",
  libraryDependencies ++= List(
    "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion.value,
    "org.specs2"       %% "specs2-core"             % "3.4"    % "test",
    "org.specs2"       %% "specs2-scalacheck"       % "3.4"    % "test",
    "org.scalacheck"   %% "scalacheck"              % "1.12.2" % "test",
    "org.typelevel"    %% "scalaz-specs2"           % "0.3.0"  % "test" exclude("org.specs2", s"specs2_${scalaBinaryVersion.value}") exclude("org.scalacheck", s"scalacheck${scalaBinaryVersion.value}"),
    "org.typelevel"    %% "shapeless-scalacheck"    % "0.3"    % "test" exclude("org.scalacheck", s"scalacheck${scalaBinaryVersion.value}")))

// ====================================================================

lazy val luschySettings =
  buildSettings ++ commonSettings

lazy val buildSettings  = List(
          organization := "de.knutwalker",
  organizationHomepage := Some(url(s"https://github.com/${githubUser.value}/")),
              homepage := Some(url(s"https://github.com/${githubUser.value}/${githubRepo.value}")),
            maintainer := "Paul Horn",
            githubUser := "knutwalker",
            githubRepo := "luschy",
           description := "Scala wrapper for Lucene",
          scalaVersion := "2.11.6",
      shapelessVersion := "2.2.0-RC4",
         luceneVersion := "5.1.0")

lazy val commonSettings = List(
  scalacOptions ++= List(
    "-deprecation",
    "-encoding",  "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-unchecked",
    "-target:jvm-1.7",
    "-Xcheckinit",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint:_",
    "-Yclosure-elim",
    "-Yconst-opt",
    "-Ydead-code",
    "-Yno-adapted-args",
    "-Ywarn-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused",
    "-Ywarn-unused-import"),
  scalacOptions in (Compile, console) ~= (_ filterNot (x ⇒ x == "-Xfatal-warnings" || x.startsWith("-Ywarn"))),
  shellPrompt := { state ⇒
    import scala.Console._
    val name = Project.extract(state).currentRef.project
    val color = GREEN
    (if (name == "parent") "" else s"[$color$name$RESET] ") + "> "
  },
  resolvers += "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases",
  cleanKeepFiles ++= List("resolution-cache", "streams").map(target.value / _),
  updateOptions ~= (_.withCachedResolution(true)),
  headers := {
    val thisYear = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR)
    val years = List(startYear.value.getOrElse(thisYear), thisYear).distinct.mkString(" – ")
    Map(
      "java"  -> Apache2_0(years, maintainer.value),
      "scala" -> Apache2_0(years, maintainer.value))
  },
  initialCommands in console := """import shapeless._; import org.apache.lucene._""",
  logBuffered := false)

lazy val publishThis = releaseSettings ++ sonatypeSettings ++ List(
                 startYear := Some(2015),
   publishArtifact in Test := false,
               tagComment <<= version map (v => s"Release version $v"),
            commitMessage <<= version map (v => s"Set version to $v"),
               versionBump := sbtrelease.Version.Bump.Minor,
  pomExtra := {
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>https://www.apache.org/licenses/LICENSE-2.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:https://github.com/{githubUser.value}/{githubRepo.value}.git</connection>
      <developerConnection>scm:git:ssh://git@github.com:{githubUser.value}/{githubRepo.value}.git</developerConnection>
      <tag>master</tag>
      <url>https://github.com/{githubUser.value}/{githubRepo.value}</url>
    </scm>
    <developers>
      <developer>
        <id>{githubUser.value}</id>
        <name>{maintainer.value}</name>
        <url>{organizationHomepage.value.get}</url>
      </developer>
    </developers>
  },
  pomPostProcess := { (node) =>
    val rewriteRule = new scala.xml.transform.RewriteRule {
      override def transform(n: scala.xml.Node): scala.xml.NodeSeq =
        if (n.label == "dependency" && (n \ "scope").text == "provided" && ((n \ "groupId").text == "org.scoverage" || (n \ "artifactId").text == "scala-reflect"))
          scala.xml.NodeSeq.Empty
        else n
    }
    val transformer = new scala.xml.transform.RuleTransformer(rewriteRule)
    transformer.transform(node).head
  },
  releaseProcess := List[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishSignedArtifacts,
    releaseToCentral,
    setNextVersion,
    commitNextVersion,
    pushChanges,
    publishArtifacts
  ))

// ====================================================================

lazy val maintainer = SettingKey[String]("Maintainer")
lazy val githubUser = SettingKey[String]("Github username")
lazy val githubRepo = SettingKey[String]("Github repository")

lazy val shapelessVersion = SettingKey[String]("Version of Shapeless")
lazy val luceneVersion = SettingKey[String]("Version of Lucene")

lazy val publishSignedArtifacts = publishArtifacts.copy(
  action = { state =>
    val extracted = Project extract state
    val ref = extracted get thisProjectRef
    extracted.runAggregated(publishSigned in Global in ref, state)
  },
  enableCrossBuild = true)

lazy val releaseToCentral = ReleaseStep(
  action = { state =>
    val extracted = Project extract state
    val ref = extracted get thisProjectRef
    extracted.runAggregated(sonatypeReleaseAll in Global in ref, state)
  },
  enableCrossBuild = true)

addCommandAlias("travis", ";clean;coverage;test;coverageReport;coverageAggregate")
