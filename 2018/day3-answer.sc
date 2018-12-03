#!/usr/bin/env scala 

val input = scala.io.Source.fromFile("day3-input.txt").getLines.toSeq

case class Claim(id: Int, x: Int, y: Int, width: Int, height: Int)
val extractor = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r
val claims = input.collect { case extractor(id, x, y, width, height) => Claim(id.toInt, x.toInt, y.toInt, width.toInt, height.toInt) }

// Part 1
val observedCoords = scala.collection.mutable.HashMap[String, Int]().withDefaultValue(0)

claims.foreach { claim =>
  for {
    x <- claim.x until claim.x + claim.width
    y <- claim.y until claim.y + claim.height
    key = s"${x}x${y}"
  } observedCoords.update(key, observedCoords(key) + 1)
}
val squareInches = observedCoords.values.filter(_ > 1).size

// Part 2
val uniqClaimId = claims.find { claim =>
  val coordCounts = for {
    x <- claim.x until claim.x + claim.width
    y <- claim.y until claim.y + claim.height
    key = s"${x}x${y}"
  } yield observedCoords(key)

  coordCounts.toSet == Set(1)
}.map(_.id).get

println("Day 3")
println("======")
println(s"Part 1: ${squareInches}")
println(s"Part 2: ${uniqClaimId}")

