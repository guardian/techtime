package com.adamnfish.packing

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets


object Main {
  def main(args: Array[String]): Unit = {
    Api.getMap().unsafeRunSync.fold (
      { failure =>
        println(failure)

        System.exit(1)
      },
      { rawMap =>
        val map = Serialisation.rawMapToMap(rawMap)
	println(map.points.map(_.label).mkString(","))
        val discs = search(map.points)
        val best50 = discs.sortBy(_.radius)(Ordering[Double].reverse).take(50)
        val score = scoreForDiscs(best50)

        val statusCode = Api.submitAnswer(score, map.mapId, best50).unsafeRunSync()
        println(s"[$statusCode] submitted to API")
        println(s"Finshed: $score ${best50.map(_.stringRepr).mkString(",")}")

        val html = Visualise.generateHtml(map, best50)
        val visualisationFile = s"/tmp/packing-${map.timestamp}.html"
        Files.write(Paths.get(visualisationFile), html.getBytes(StandardCharsets.UTF_8))
        println(s"Visualisation: file://$visualisationFile")

        System.exit(0)
      }
    )
  }

  def search(allPoints: List[Point]): List[Disc] = {
    def loop(discs: List[Disc], remainingPoints: List[Point]): List[Disc] = {
      remainingPoints
        .map { point =>
          point -> maxRadius(point, allPoints, discs)
        }
        .sortBy(_._2)(Ordering[Double].reverse).headOption match {
          case None =>
            discs
          case Some((point, radius)) =>
            loop(Disc(point, radius) :: discs, remainingPoints.filterNot(_ == point))
        }
    }
    loop(Nil, allPoints)
  }

  def maxRadius(point: Point, allPoints: List[Point], discs: List[Disc]): Double = {
    val discConstraint = discs.map { disc =>
      distanceBetween(point, disc.point) - disc.radius
    }.sorted.headOption.getOrElse(1000D)

    val pointConstraint = distanceBetween(point, nearestNeighbour(point, allPoints))

    math.min(discConstraint, pointConstraint)
  }

  def distanceBetween(p1: Point, p2: Point): Double = {
    math.sqrt(
      math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2)
    )
  }

  def nearestNeighbour(point: Point, allPoints: List[Point]): Point = {
    allPoints.filterNot(_ == point).minBy(distanceBetween(_, point))
  }

  def scoreForDisc(disc: Disc): Double = {
    math.pow(disc.radius, 2)
  }

  def scoreForDiscs(discs: List[Disc]): Double = {
    discs.map(scoreForDisc).sum
  }
}
