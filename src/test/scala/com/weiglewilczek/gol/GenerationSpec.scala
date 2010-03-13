/**
 * Copyright (c) 2010 WeigleWilczek.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 */
package com.weiglewilczek.gol

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class GenerationSpec extends WordSpec with ShouldMatchers {

  "Constructing a Generation" when {
    "the given alive argument in null" should {
      "throw an IllegalArgumentException" in {
        evaluating { new Generation(null: Set[Cell]) } should produce [IllegalArgumentException]
      }
    }
    "constructed without alive argument" should {
      "contain an empty sequence of alive cells" in {
        new Generation().alive should be (Set.empty)
      }
    }
  }

  "Calling Generation.next" when {
    "the generation contains an empty sequence of alive cells" should {
      "return an empty generation again" in {
        new Generation().next.alive should be (Set.empty)
      }
    }
    "the generation contains only one alive cell" should {
      "return an empty generation" in {
        val Alive = Set(Cell(0, 0))
        new Generation(Alive).next.alive should be (Set.empty)
      }
    }
    "the generation contains only two alive cells" should {
      "return an empty generation" in {
        val Alive = Set(Cell(0, 0), Cell(0, 1))
        new Generation(Alive).next.alive should be (Set.empty)
      }
    }
    "the generation contains three alive cells in a horizontal row" should {
      "return three alive cells in a vertical row" in {
        val Horizontal3 = Set(Cell(0, -1), Cell(0, 0), Cell(0, 1))
        val Vertical3 = Set(Cell(-1, 0), Cell(0, 0), Cell(1, 0))
        new Generation(Horizontal3).next.alive should be (Vertical3)
      }
    }
    "the generation contains three alive cells in a diagonal row" should {
      "return only one alive cells" in {
        val Diagonal3 = Set(Cell(-1, -1), Cell(0, 0), Cell(1, 1))
        new Generation(Diagonal3).next.alive should be (Set(Cell(0, 0)))
      }
    }
    "the generation contains four alive cells as a diamond" should {
      "return an unmodified generation" in {
        val Diamond2 = Set(Cell(0, 1), Cell(1, 0), Cell(0, -1), Cell(-1, 0))
        new Generation(Diamond2).next.alive should be (Diamond2)
      }
    }
    "the generation contains eight alive cells as 3x3 square" should {
      "return eight cells as 3x3 diamond" in {
        val Square3 = Set(Cell(-1, -1), Cell(-1, 0), Cell(-1, 1),
                          Cell(0, -1), Cell(0, 1),
                          Cell(1, -1), Cell(1, 0), Cell(1, 1))
        val Diamond3 = Set(Cell(-2, 0), Cell(2, 0), Cell(0, -2), Cell(0, 2),
                           Cell(-1, -1), Cell(-1, 1), Cell(1, -1), Cell(1, 1))
        new Generation(Square3).next.alive should be (Diamond3)
      }
    }
  }
}
