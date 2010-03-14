/**
 * Copyright (c) 2010 WeigleWilczek.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 */
package com.weiglewilczek.gol

import scala.collection.Set

/**
 * A generation of alive cells, empty by default.
 */
class Generation(val alive: Set[Cell] = Set.empty) {
  require(alive != null, "Illegal argument: alive must not be null!")

  /**
   * Transitions to the next generation by applying Conway's standard rules.
   */
  def next: Generation = {
    val stayingAlive = alive filter { (2 to 3) contains aliveNeighbours(_).size  }
    val wakingFromDead = alive flatMap deadNeighbours filter { aliveNeighbours(_).size == 3 }
    new Generation(stayingAlive ++ wakingFromDead)
  }

  /**
   * Creates an ASCII art representation.
   */
  override def toString: String = {
    val rows = (dimension._3 to dimension._4).reverse map { y =>
      val row = dimension._1 to dimension._2 map { x =>
        if (alive contains Cell(x, y)) "X" else "-"
      }
      row.mkString
    }
    rows mkString "\n"
  }

  private lazy val dimension =
    ((alive map { _.x }).min,
     (alive map { _.x }).max,
     (alive map { _.y }).min,
     (alive map { _.y }).max)

  private def neighbours(cell: Cell) =
    for {
      x <- (cell.x-1) to (cell.x+1)
      y <- (cell.y-1) to (cell.y+1) if (x != cell.x) || (y != cell.y)
    } yield Cell(x, y)

  private def aliveNeighbours(cell: Cell) = neighbours(cell) filter { alive contains _ }

  private def deadNeighbours(cell: Cell) = neighbours(cell) filter { cell => !(alive contains cell) }
}
