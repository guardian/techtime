lazy val root = (project in file(".")).
  settings(
    name := "20180913-salesman-energy",

    inThisBuild(List(
      organization := "com.adamnfish.techtime",
      scalaVersion := "2.12.6",
      version := "0.1.0-SNAPSHOT"
    )),

    libraryDependencies ++= Seq(
      "com.criteo.lolhttp" %% "loljson" % "0.10.1",
      "io.circe" %% "circe-core" % "0.9.3",
      "io.circe" %% "circe-generic" % "0.9.3",
      "io.circe" %% "circe-parser" % "0.9.3",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    ),

    mainClass in assembly := Some("com.adamnfish.techtime.Main"),
    assemblyJarName in assembly := s"${name.value}.jar",
    test in assembly := {}
  )
