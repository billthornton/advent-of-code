#!/usr/bin/env scala

val input = scala.io.Source.fromFile("day5-input.txt").getLines.toSeq.head

def polymerReduce(in: String): String = { 
  in.scan("") { 
    case (a, b) => {
      val aa = a.toString
      val lastA = if (aa.nonEmpty) aa.last.toString else ""
      val bb = b.toString
      
      if (lastA != bb && lastA.toLowerCase == bb.toLowerCase) aa.take(aa.size - 1) else s"$aa$bb"
    }
  }.last.toString
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

