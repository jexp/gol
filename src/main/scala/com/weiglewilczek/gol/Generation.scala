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
 * Utilities for a generation of cells.
 */
object Generation {

  /**
   * String representation for alive.
   */
  val Alive = "X"

  /**
   * String representation for dead.
   */
  val Dead = "-"

  /**
   * Creates a generation
   */
  def fromString2(generation: String): Generation = {
    require(generation != null, "Illegal argument: generation must not be null!")
    val lines = generation.lines.toSeq.reverse
    val alive = for {
      y <- 0 until lines.size
      line = lines(y)
      x <- 0 until line.size if line(x).toString == Alive
    } yield Cell(x, y)
    new Generation(alive.toSet)
  }
  def fromString(generation: String): Generation = {
    require(generation != null, "Illegal argument: generation must not be null!")
    val (_,_,alive) = ((0,0,Set[Cell]()) /: generation) { (t,c) =>
	    val (x,y,r) = t
		c match {
			case 'X'  => (x+1, y  , r + Cell(x,y))
			case '\n' => (0  , y-1, r)
			case _    => (x+1, y  , r)
		}
    }
	// just for the existing test cases
	
	
	val minY = (alive map { _.y }).min
	val alive2 = alive.map { c => Cell(c.x,c.y-minY)}.toSet
    new Generation(alive2)
  }
}

/**
 * A generation of alive cells, empty by default.
 */
class Generation(val alive: Set[Cell] = Set.empty) {
  require(alive != null, "Illegal argument: alive must not be null!")

  def next: Generation = {
    val stayingAlive = alive filter { (2 to 3) contains aliveNeighbours(_).size  }
    val wakingFromDead = alive flatMap deadNeighbours filter { aliveNeighbours(_).size == 3 }
    new Generation(stayingAlive ++ wakingFromDead)
  }

  private def neighbours(cell: Cell) =
    for {
      dx <- (-1) to (1)
      dy <- (-1) to (1) 
      n = cell[dx,dy]
	  if (n!=cell)
    } yield n

  private def aliveNeighbours(cell: Cell) = neighbours(cell) filter { alive contains _ }

  private def deadNeighbours(cell: Cell) = neighbours(cell) filter { cell => !(alive contains cell) }

  override def toString: String = {
    val rows = (dimension._3 to dimension._4).reverse map { y =>
      val row = dimension._1 to dimension._2 map { x =>
        if (alive contains Cell(x, y)) Generation.Alive else Generation.Dead
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
}
