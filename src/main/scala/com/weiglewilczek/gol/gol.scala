/**
 * Copyright (c) 2010 WeigleWilczek.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 */
package com.weiglewilczek.gol

/**
 * A cell on the board with coordinates x and y.
 */
case class Cell(x: Int, y: Int)

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

  private def neighbours(cell: Cell) =
    for {
      i <- (cell.x-1) to (cell.x+1)
      j <- (cell.y-1) to (cell.y+1) if (i != cell.x) || (j != cell.y)
    } yield Cell(i, j)

  private def aliveNeighbours(cell: Cell) = neighbours(cell) filter { alive contains _ }

  private def deadNeighbours(cell: Cell) = neighbours(cell) filter { cell => !(alive contains cell) }
}
