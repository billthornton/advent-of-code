#!/usr/bin/env scala

val input = scala.io.Source.fromFile("day4-input.txt").getLines.toSeq

val GuardExtractor = ".* Guard #(\\d+) begins shift".r

val (shifts: Map[Int, Seq[String]], _) = input
  .sorted
  .foldLeft((Map[Int, Seq[String]]().withDefaultValue(Nil), Option.empty[Int])) {
    case ((map, currentGuardId), GuardExtractor(guardId)) => {
      (map, Some(guardId.toInt))
    }
    case ((map, Some(currentGuardId)), line) =>
      (map.updated(currentGuardId, map(currentGuardId) :+ line), Some(currentGuardId))
}

val TimeParse = ".* 00:(\\d+)\\].*".r

def getTime(logLine: String) = logLine match {
  case TimeParse(minutes) => minutes.toInt
}

val shiftSleeps = shifts
  .toSeq
  .map { case (guardId, logs) => 
    guardId -> logs.grouped(2).toSeq 
  }
  .map {
    case (guardId, logPairs) => {
      val sleepRanges = logPairs.map {
        case Seq(asleepLog, wakeLog) => getTime(asleepLog) until getTime(wakeLog)
      }
      guardId -> sleepRanges
    }
  }
  .toMap

val (mostSleptGuard, _) = shiftSleeps
  .toSeq
  .map { case (guardId, sleepRanges) =>
    (guardId, sleepRanges.map(_.size).sum)
  }
  .sortBy { case (_, totalSleepTime) => totalSleepTime }
  .reverse
  .head

def getSleepMostSleptMinute(sleepTimes: Seq[Range]): (Int, Int) = {
  val sleepPattern = scala.collection.mutable.HashMap[Int, Int]().withDefaultValue(0)
  for {
    range <- sleepTimes
    minute <- range
  } sleepPattern.update(minute, sleepPattern(minute) + 1)

  sleepPattern
    .toSeq
    .sortBy { case (_, minuteFrequency) => minuteFrequency }
    .reverse
    .head
}

val sleepTimes = shiftSleeps(mostSleptGuard)
val (mostSleptMinute, _) = getSleepMostSleptMinute(sleepTimes)
val answer1 = mostSleptMinute * mostSleptGuard

val (guard2Id, (guard2Min, _)) = shiftSleeps
  .toSeq
  .map { case (guardId, guardSleepTimes) => guardId ->
    getSleepMostSleptMinute(guardSleepTimes) }
  .sortBy { case (_, (_, count)) => count }
  .reverse
  .head

val answer2 = guard2Id * guard2Min


println("Day 4")
println("======")
println(s"Part 1: ${answer1}")
println(s"Part 2: ${answer2}")

