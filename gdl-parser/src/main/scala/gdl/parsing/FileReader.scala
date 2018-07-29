package gdl.parsing

import gdl.validation.dependencyGraph.Relation
import gdl.validation.{DependencyGraph, Restrictions}

import scala.io.Source

object FileReader {
  def main(args: Array[String]): Unit = {
    val description = Source.fromResource("tic-tac-toe-infix.gdl").mkString

    for (ast <- InfixGdlParser(description)) {
      val restrictions = new Restrictions(ast)
      print(restrictions.validateUniqueNames)
      print(restrictions.validateDoesKeyword)
      print(restrictions.validateInitKeyword)
      print(restrictions.validateStratification)
      print(restrictions.validateRecursion)

      val dependencyGraph = new DependencyGraph(ast)
      println(dependencyGraph.relations.mkString("\n"))
      println("***")
      println(restrictions.stratify match {
        case Left(validationMessages) => print(validationMessages)
        case Right(strata) => for (i <- strata.keys.toList.sorted) println(s"Stratum $i:\n${strata(i).mkString("\n")}")
      })
    }
  }

  private def print(messages: Set[String]) = {
    if (messages.isEmpty) println("OK")
    else println(messages.mkString("\n"))
  }

  private def pathToString(count: Int, relations: Set[Relation]): String =
    s"stratum $count: ${relations.map(_.toRelationConstant).mkString(", ")}"
}
