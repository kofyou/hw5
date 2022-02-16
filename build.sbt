// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "UCSC-AHD"

val chiselVersion = "3.5.0"

// For running the gradescope tests
libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.10.0" % "test"

// This sets it up so all tests that end in "Tester" will be run when you run sbt test
// and all tests that end in "Grader" will run when you run sbt Grader / test
lazy val scalatest = "org.scalatest" %% "scalatest" % "3.2.10"
lazy val Grader = config("grader") extend(Test)
lazy val TestAll = config("testAll") extend(Test)
lazy val Hw5c = config("hw5c") extend(Test)

def allFilter(name: String): Boolean = name endsWith "Tester"
def graderFilter(name: String): Boolean = name endsWith "Grader"
def hw5cFilter(name: String): Boolean = name endsWith "Testerhw5c"

lazy val root = (project in file("."))
.configs(TestAll).configs(Grader).configs(Hw5c)
  .settings(
    name := "hw5c",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.0" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
    // from dino
    inConfig(Grader)(Defaults.testTasks),
    inConfig(TestAll)(Defaults.testTasks),
    inConfig(Hw5c)(Defaults.testTasks),

    libraryDependencies += scalatest % TestAll,
    libraryDependencies += scalatest % Grader,
    libraryDependencies += scalatest % Hw5c,

    testOptions in TestAll := Seq(Tests.Filter(allFilter)),
    // CHANGE THE LINE BELOW FOR EACH LAB!!!! Use the matching filter
    testOptions in Test := Seq(Tests.Filter(allFilter)),
    testOptions in Grader := Seq(Tests.Filter(graderFilter)),
    testOptions in Hw5c := Seq(Tests.Filter(hw5cFilter)),
 )
