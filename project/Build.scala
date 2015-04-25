import sbt.{Developer => _, _}
import sbt.Keys._
import de.knutwalker.sbt._
import de.knutwalker.sbt.KSbtKeys._


object Build extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = KSbtPlugin

  override lazy val projectSettings = List(
      organization := "de.knutwalker",
         startYear := Some(2015),
        maintainer := "Paul Horn",
     githubProject := Github("knutwalker", "luschy"),
       description := "Scala wrapper for Lucene",
      scalaVersion := "2.11.6",
     luceneVersion := "5.1.0",
  shapelessVersion := "2.2.0-RC4"
  ) ++ List(
    initialCommands in      console := """import shapeless._; import org.apache.lucene._; import luschy._, Luschy._""",
    initialCommands in consoleQuick := """import shapeless._; import org.apache.lucene._"""
  )
}
