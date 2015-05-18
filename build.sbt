lazy val parent = project in file(".") dependsOn core aggregate (core, tests) settings dontRelease

lazy val core = project settings (
  name := "luschy",
  libraries += Library.Lucene.core and Library.Shapeless.at("2.2.0-RC6"),
  libraryDependencies += "de.knutwalker" %% "validation" % "0.2.0")

lazy val tests = project dependsOn core settings (
  dontRelease,
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"
  ),
  libraries ++= List(
    Library.Lucene.`analyzers-common`(Test),
    Library.Specs2.core(Test).scalacheck(Test).at("3.6")),
  libraryDependencies ++= List(
    "org.scalacheck"             %% "scalacheck"                % "1.12.2"           % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.12" % "0.2.0-SNAPSHOT"   % "test",
    "org.typelevel"              %% "scalaz-specs2"             % "0.4.0"            % "test"
      exclude("org.specs2", s"specs2-core${scalaBinaryVersion.value}")
      exclude("org.specs2", s"specs2-scalacheck${scalaBinaryVersion.value}")
  ))

addCommandAlias("travis", ";clean;coverage;test;coverageReport;coverageAggregate")
