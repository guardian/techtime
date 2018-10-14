package com.adamnfish.packing

import org.scalatest.{FreeSpec, Matchers}

class MainTest extends FreeSpec with Matchers {
  import Main.maxRadius

  "maxRadius" - {
    val point = Point("test", 0, 0)

    "calculates it correctly for just nearest neighbour" in {
      maxRadius(point, List(Point("1", 1, 0), Point("2", 2, 0)), Nil) shouldEqual 1D
    }

    "calculates it correctly with a disc constraint" in {
      val discPoint = Point("2", 2, 0)
      maxRadius(point,
        List(Point("1", 1, 0), discPoint),
        List(Disc(discPoint, 1.5))
      ) shouldEqual 0.5D
    }

    "gives radius 0 if a disc is already 'touching'" in {
      val discPoint = Point("2", 2, 0)
      maxRadius(point,
        List(Point("1", 1, 0), discPoint),
        List(Disc(discPoint, 2))
      ) shouldEqual 0D
    }
  }
}
