package com.adamnfish.techtime

import org.scalatest.{FreeSpec, Matchers}

import Main._
import math.pow


class MainTest extends FreeSpec with Matchers {
  "reachable" - {
    "does not return a point too far away" in {
      reachable(1D, Stop("1", 0, 0, 5), Set(Stop("2", 0, 5, 5))) shouldEqual Nil
    }

    "returns a point just reachable" in {
      reachable(1D, Stop("1", 0, 0, 5), Set(Stop("2", 0, 1, 5))) shouldEqual List(Stop("2", 0, 1, 5) -> 1)
    }

    "finds just the reachable points, ordered by distance" in {
      val stop1 = Stop("1", 0, 1, 5)
      val stop2 = Stop("2", 0, 2, 5)
      val stop3 = Stop("3", 0, 3, 5)
      val stop4 = Stop("4", 0, 4, 5)
      val candidates = Set(stop1, stop2, stop3, stop4)
      reachable(pow(3D, 2), Stop("1", 0, 0, 5), candidates) shouldEqual List(stop3 -> 3, stop2 -> 2, stop1 -> 1)
    }
  }

  "distanceBetween" - {
    "returns zero for points at the same place" in {
      val stop1 = Stop("1", 0, 0, 0)
      val stop2 = Stop("2", 0, 0, 0)
      distanceBetween(stop1, stop2) shouldEqual 0
    }

    "returns correct value for two points" in {
      val stop1 = Stop("1", 0, 0, 0)
      val stop2 = Stop("2", 0, 2, 0)
      distanceBetween(stop1, stop2) shouldEqual 2
    }

    "returns correct value for two points in 2 dimensions" in {
      val stop1 = Stop("1", 4, 0, 0)
      val stop2 = Stop("2", 0, 3, 0)
      distanceBetween(stop1, stop2) shouldEqual 5
    }
  }

  "search" - {
    def report(score: Double, pathIds: String): Unit = ()

    "works for toy example" in {
      val stop0 = Stop("0", 0, 0, 2)
      val stop1 = Stop("1", 1, 0, 1)
      val stop2 = Stop("2", 2, 0, 1)
      val stop3 = Stop("3", 3, 0, 1)
      val stop4 = Stop("4", 4, 0, 1)
      val stops = List(stop0, stop1, stop2, stop3, stop4)
      search(stops, report) shouldEqual (4, stops)
    }

    "works for more complicated example" in {
      val stop0 = Stop("0", 0, 0, 2)
      val stop1 = Stop("1", 1, 0, 1)
      val stop2 = Stop("2", 2, 0, 1)
      val stop3 = Stop("3", 3, 0, 1)
      val stop4 = Stop("4", 4, 0, 1)
      val stop5 = Stop("5", 5, 0, 1)
      val stop6 = Stop("6", 6, 0, 1)
      val stop7 = Stop("7", 7, 0, 1)
      val stop8 = Stop("8", 8, 0, 1)
      val stop9 = Stop("9", 9, 0, 1)
      val stops = List(stop0, stop1, stop2, stop3, stop4, stop5, stop6, stop7, stop8, stop9)
      search(stops, report) shouldEqual (9, stops)
    }
  }

  "doubleCheck" - {
    "works for toy example" in {
      val stop0 = Stop("0", 0, 0, 1)
      val stop1 = Stop("1", 1, 0, 1)
      val stop2 = Stop("2", 2, 0, 1)
      val stop3 = Stop("3", 3, 0, 1)
      val stop4 = Stop("4", 4, 0, 1)
      val stops = List(stop0, stop1, stop2, stop3, stop4)
      doubleCheck(stops) shouldEqual 4D
    }
  }
}
