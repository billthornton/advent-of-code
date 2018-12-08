#!/usr/bin/env scala

val filename = "day7-input.txt"
val input = scala.io.Source.fromFile(filename).getLines.toSeq

val EdgeExtractor = "Step (\\w) must be finished before step (\\w) can begin.".r
val stepRequirements = input.map { case EdgeExtractor(prevStep, step) => prevStep -> step }

val mapping = stepRequirements.groupBy(_._1).toSeq.map { case (key, values) => key -> values.map(_._2).sorted }.toMap
val altMapping = stepRequirements.groupBy(_._2).toSeq.map { case (key, values) => key -> values.map(_._1).sorted }.toMap
val starting = (mapping.keySet diff altMapping.keySet)

def process(output: Seq[String], candidates: Seq[String]): Seq[String] = {
  val sorted = candidates.sorted
  val next = sorted.find(a => (altMapping.getOrElse(a, Nil) diff output) == Nil).get
  val rest = sorted.filter(a => a != next)
  val newOutput = output :+ next

  if (mapping.contains(next)) {
    process(newOutput, rest ++ mapping(next))
  } else newOutput
}

val answer1 = process(Nil, starting.toSeq).mkString

val MaxWorkers = 5
val startingWorkers = Range(0, MaxWorkers).map(_ => (0, Option.empty[String]))
val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

def multiProcess(output: Seq[String], candidates: Seq[String], workQueue: Seq[(Int, Option[String])], runs: Int = 0): Int = {
  val (finishedWorkers, busyWorkers) = workQueue.partition { case (timeLeft, _) => timeLeft == 0 }
  val processedBusyWorkers = busyWorkers.map { case (timeLeft, letter) => (timeLeft - 1, letter) }

  val finishedLetters = finishedWorkers.flatMap(_._2)
  val (nextToLookUp, endLetters) = finishedLetters.partition(mapping.contains)
  val nextLetters = nextToLookUp.flatMap(mapping)

  val completedLetters = output ++ finishedLetters
  val possibleCandidates = (candidates ++ nextLetters).toSet.toSeq.sorted
  val (availableLetters, rest) = possibleCandidates.partition(a => (altMapping.getOrElse(a, Nil) diff completedLetters) == Nil)

  val assignedWorkers = availableLetters.zip(finishedWorkers).map { case (letter, _) => (alphabet.indexOf(letter) + 60, Some(letter)) }
  val unprocessedLetters = availableLetters diff assignedWorkers.flatMap(_._2)
  val unassignedWorkers = Range(0, finishedWorkers.size - assignedWorkers.size).map(_ => (0, None))

  val newCandidates = unprocessedLetters ++ rest
  val newWorkQueue = processedBusyWorkers ++ assignedWorkers ++ unassignedWorkers

  if (endLetters.nonEmpty) {
    runs
  } else {
    multiProcess(completedLetters, newCandidates, newWorkQueue, runs + 1)
  }
}

val answer2 = multiProcess(Nil, starting.toSeq, startingWorkers)

println("Day 7")
println("======")
println(s"Part 1: ${answer1}")
println(s"Part 2: ${answer2}")

