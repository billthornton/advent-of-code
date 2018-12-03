#!/usr/bin/env scala 

val input = scala.io.Source.fromFile("day1-input.txt").getLines.toSeq
val frequencyChanges = input.map(_.toInt)

// Part 1
val answer1 = frequencyChanges.sum

val continuousFrequencies = Stream.continually(frequencyChanges).flatten

// Part 2: Attempt 1 (slow)
def findRepetition(index: Int, observedValues: Seq[Int], currentFreq: Int, stream: Stream[Int]): Int = {
      val newValue = stream(index) + currentFreq
      if (observedValues.contains(newValue)) {
        return newValue
      }
      findRepetition(index + 1, observedValues :+ newValue, newValue, stream)
    }

//val answer2 = findRepetition(0, Seq(0), 0, continuousFrequencies)

// Part 2: Attempt 2
val answer2 = continuousFrequencies.scanLeft((Set[Int](), 0)) {
  case ((observedValues, lastFrequency), newFrequency) =>
    (observedValues + lastFrequency) -> (lastFrequency + newFrequency)
}.collectFirst { 
  case (observations, frequency) if observations.contains(frequency) => frequency
}

println("Day 1")
println("=====")
println(s"Part 1: ${answer1}")
println(s"Part 2: ${answer2.get}")

