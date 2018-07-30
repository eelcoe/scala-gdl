import scalariform.formatter.preferences._

scalaVersion in ThisBuild := "2.12.4"
scalariformPreferences in ThisBuild := scalariformPreferences.value
  .setPreference(CompactControlReadability, true)
  .setPreference(DanglingCloseParenthesis, Force)
  .setPreference(NewlineAtEndOfFile, true)
  .setPreference(PreserveSpaceBeforeArguments, true)
  .setPreference(SingleCasePatternOnNewline, false)
  .setPreference(SpacesAroundMultiImports, false)
  .setPreference(SpaceBeforeContextColon, true)

lazy val description = (project in file("gdl-description"))

lazy val parser = (project in file("gdl-parser"))
  .dependsOn(description % "test->test;compile->compile")

lazy val propnet = (project in file("gdl-propnet"))
  .dependsOn(description % "test->test;compile->compile")
