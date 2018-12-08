#!/usr/bin/env scala

val filename = "day8-input.txt"
val input = scala.io.Source.fromFile(filename).getLines.toSeq.head.split(" ").map(_.toInt)

def getMetadata(in: Seq[Int]): (Seq[Int], Seq[Int]) = {
  val (Seq(childCount, metadataCount), rest) = in.splitAt(2)

  val (leftOver, allChildMeta) = (0 until childCount)
    .foldLeft((rest, Seq[Int]())) { case ((accRest, meta), _) => {
      if (accRest.nonEmpty) {
        val (childMetadata, childTail) = getMetadata(accRest)
        childTail -> (childMetadata ++ meta)
      } else accRest -> meta
    }
  }
  val (parentMeta, afterText) = leftOver.splitAt(metadataCount)

  (parentMeta ++ allChildMeta) -> afterText
}

val (metadata, _) = getMetadata(input)

val answer1 = metadata.sum

println("Day 8")
println("======")
println(s"Part 1: ${answer1}")
println(s"Part 2: ")

