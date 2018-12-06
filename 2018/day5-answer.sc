#!/usr/bin/env scala

val input = scala.io.Source.fromFile("day5-input.txt").getLines.toSeq.head

def polymerReduce(in: String): String = { 
  in
    .toSeq
    .map(_.toString)
    .reduce {
      (acc, candidate) => {
        val lastCandidate = if (acc.nonEmpty) acc.last.toString else ""
        if (lastCandidate != candidate && lastCandidate.toLowerCase == candidate.toLowerCase) acc.take(acc.size - 1) else s"$acc$candidate"
      }
    }
}

val result1 = polymerReduce(input).size

val chars = input.toLowerCase.toSet.toSeq

val perCharSizes = chars.map { char =>
    char -> polymerReduce(input.replaceAll(s"(?i)${char}", "")).toString.size
  }

val result2 = perCharSizes.sortBy(_._2).head._2

println("Day 5")
println("======")
println(s"Part 1: ${result1}")
println(s"Part 2: ${result2}")

