#!/usr/bin/env scala

val input = scala.io.Source.fromFile("day6-input.txt").getLines.toSeq

val coords = input.map(_.split(", ").map(_.toInt)).map(a => a(0) -> a(1))

val maxX = coords.map(_._1).max + 1
val minX = coords.map(_._1).min - 1
val minY = coords.map(_._2).min - 1
val maxY = coords.map(_._2).max + 1

val grid = scala.collection.mutable.HashMap[(Int, Int), (Int, Int)]()

for {
  ((coordX, coordY), name) <- coords.zipWithIndex
  x <- minX until maxX
  y <- minY until maxY
  distance = Math.abs(x - coordX) + Math.abs(y - coordY)
  (existingDistance, existingName) = grid.getOrElse((x, y), (-1, -1))
  if (existingDistance == -1 || distance <= existingDistance)
} if (distance == existingDistance) grid.update((x, y), (distance, -1)) else grid.update((x, y), (distance, name))

val minXNames = grid.toSeq.collect { case ((x, _), (_, name)) if x == minX => name }.toSet
val minYNames = grid.toSeq.collect { case ((_, y), (_, name)) if y == minY => name }.toSet
val maxYNames = grid.toSeq.collect { case ((_, y), (_, name)) if y == (maxY - 1) => name }.toSet
val maxXNames = grid.toSeq.collect { case ((x, _), (_, name)) if x == (maxX - 1) => name }.toSet
val infinites = (minXNames ++ minYNames ++ maxYNames ++ maxXNames)

val answer1 = grid
  .toSeq
  .collect { 
    case (_, (_, name)) if !infinites.contains(name) => name
  }
  .groupBy(identity)
  .toSeq
  .sortBy(a => a._2.size)
  .reverse
  .map { case (name, array) => name -> array.size }
  .head
  ._2

println("Day 6")
println("======")
println(s"Part 1: ${answer1}")
println(s"Part 2: ")

