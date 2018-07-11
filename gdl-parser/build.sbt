name := "gdl-parser"

version := "0.1"

libraryDependencies ++= Seq(
  "com.michaelpollmeier" %% "gremlin-scala" % "3.3.1.1",
  "org.apache.tinkerpop" % "tinkergraph-gremlin" % "3.3.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)
