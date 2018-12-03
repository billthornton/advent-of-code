#!/usr/bin/env scala 

val input = scala.io.Source.fromFile("day2-input.txt").getLines.toSeq

// Part 1
def findRepetitions(repeatSize: Int)(line: String) = line
  .split("")
  .groupBy(a => a)
  .collectFirst { case (key, values) if values.size == repeatSize => 
    true
  }.isDefined

val doubleCount = input.filter(findRepetitions(2)).size
val tripleCount = input.filter(findRepetitions(3)).size
val checksum = doubleCount * tripleCount

// Part 2
val commonChars = input
  .combinations(2)
  .map { case Seq(candidateA, candidateB) =>
    candidateA
      .split("")
      .zip(candidateB.split(""))
      .partition { case (charA, charB) => charA == charB }
  }
  .collectFirst { 
    case (intersections, diffs) if diffs.size == 1 =>
      intersections.map(_._1).mkString
  }.getOrElse("Not found")

println("Day 2")
println("======")
println(s"Part 1: ${checksum}")
println(s"Part 2: ${commonChars}")

