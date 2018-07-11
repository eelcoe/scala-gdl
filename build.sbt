scalaVersion in ThisBuild := "2.12.4"

lazy val description = (project in file("gdl-description"))

lazy val parser = (project in file("gdl-parser"))
  .dependsOn(description % "test->test;compile->compile")

lazy val propnet = (project in file("gdl-propnet"))
  .dependsOn(description % "test->test;compile->compile")
