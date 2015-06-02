lazy val parent = project in file(".") dependsOn (model, json, core) aggregate (model, json, core, tests) settings dontRelease

lazy val model = project settings (
  name := "luschy-model",
  libraryDependencies += "de.knutwalker" %% "validation" % "0.2.0")

lazy val json = project dependsOn model settings (
  name := "luschy-json",
  libraryDependencies ++= List(
    "io.argonaut"   %% "argonaut"   % "6.1",
    "de.knutwalker" %% "validation" % "0.2.0"))

lazy val core = project settings (
  name := "luschy",
  libraryDependencies ++= List(
    "de.knutwalker"     %% "validation"  % "0.2.0",
    "org.apache.lucene"  % "lucene-core" % "5.1.0",
    "com.chuusai"       %% "shapeless"   % "2.2.0-RC6"
  ))

lazy val tests = project dependsOn core settings (
  dontRelease,
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"
  ),
  libraryDependencies ++= List(
    "org.apache.lucene"           % "lucene-analyzers-common"   % "5.1.0"            % "test",
    "org.specs2"                 %% "specs2-core"               % "3.6"              % "test",
    "org.specs2"                 %% "specs2-scalacheck"         % "3.6"              % "test",
    "org.scalacheck"             %% "scalacheck"                % "1.12.2"           % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.12" % "0.2.0-SNAPSHOT"   % "test",
    "org.typelevel"              %% "scalaz-specs2"             % "0.4.0"            % "test"
      exclude("org.specs2", s"specs2-core${scalaBinaryVersion.value}")
      exclude("org.specs2", s"specs2-scalacheck${scalaBinaryVersion.value}")
  ))

addCommandAlias("travis", ";clean;coverage;test;coverageReport;coverageAggregate")
