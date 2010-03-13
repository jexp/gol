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
        evaluating { new Generation(null) } should produce [IllegalArgumentException]
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
        new Generation(Set(Cell(0, 0))).next.alive should be (Set.empty)
      }
    }
    "the generation contains only two alive cells" should {
      "return an empty generation" in {
        new Generation(Set(Cell(0, 0), Cell(0, 1))).next.alive should be (Set.empty)
      }
    }
    "the generation contains three alive cells in a row" should {
      "return three alive cells in a column" in {
        val Alive = Set(Cell(0, -1), Cell(0, 0), Cell(0, 1))
        new Generation(Alive).next.alive should be (Set(Cell(-1, 0), Cell(0, 0), Cell(1, 0)))
      }
    }
  }
}
