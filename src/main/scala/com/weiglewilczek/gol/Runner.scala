package com.weiglewilczek.gol

object Runner {

  def main(args: Array[String]) {
    val generation = if (args.size == 0) DefaultGenerationZero else args(0)
    printGeneration(Generation fromString generation)
  }

  private def printGeneration(generation: Generation) {
    println(generation)
    println()
    Thread sleep 1000
    val nextGeneration = generation.next
    if (generation.alive != nextGeneration.alive) printGeneration(nextGeneration)
  }

  private lazy val DefaultGenerationZero =
    "XXX" + LineSeparator +
    "X-X" + LineSeparator +
    "XXX"

  private lazy val LineSeparator = System.getProperty("line.separator", "\n")
}
