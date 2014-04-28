scalaSource in Compile := new File(baseDirectory.value, "main")

scalaSource in Test := new File(baseDirectory.value, "test")

scalacOptions := Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

scalaVersion := "2.11.0" // required for scalatest

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.4" % "test"
