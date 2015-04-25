lazy val parent = project in file(".") dependsOn core aggregate (core, tests) settings dontRelease

lazy val core = project settings (name := "luschy")

lazy val tests = project dependsOn core settings (
  name := "luschy-tests",
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"
  ),
  libraryDependencies ++= List(
    "org.apache.lucene"           % "lucene-analyzers-common"   % luceneVersion.value % "test",
    "org.specs2"                 %% "specs2-core"               % "3.4"              % "test",
    "org.specs2"                 %% "specs2-scalacheck"         % "3.4"              % "test",
    "org.scalacheck"             %% "scalacheck"                % "1.12.2"           % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.12" % "0.2.0-SNAPSHOT"   % "test",
    "org.typelevel"              %% "scalaz-specs2"             % "0.4.0"            % "test"
      exclude("org.specs2", s"specs2-core${scalaBinaryVersion.value}")
      exclude("org.specs2", s"specs2-scalacheck${scalaBinaryVersion.value}")
  ))

addCommandAlias("travis", ";clean;coverage;test;coverageReport;coverageAggregate")
