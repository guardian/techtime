package com.adamnfish.techtime

import scala.math.{abs, pow, sqrt}


object Main {
  def main(args: Array[String]): Unit = {
    Api.getMap().unsafeRunSync.fold (
      { failure =>
        println(failure)
      },
      { map =>
        println(map.mapId)
        val stops = map.points.map(Serialisation.pointToStop).filter(_.energy >= 0)

        // ensure some points in case the search space is too large?
        // (impure function for now instead)

        val (bestScore, bestPath) = search(stops, { case (score, pathIds) =>
          println(s"submitting new best... $score $pathIds")
          val status = Api.submitAnswer(score, map.mapId, pathIds).unsafeRunSync()
          println(s"[$status] $score $pathIds")
        })

        println(s"Finshed: $bestScore $bestPath")
      }
    )
  }

  def search(stops: List[Stop], report: (Double, String) => Unit): (Double, List[Stop]) = {
    var best = 0D

//    val badEdges = longEdges(allEdges(stops))

    def loop(current: Stop, energy: Double, score: Double, visited: List[Stop], notVisited: Set[Stop]): (Double, List[Stop]) = {
      val candidatesForNextStop = reachable(energy + current.energy, current, notVisited)
      if (candidatesForNextStop.isEmpty) {
        if (score > best) {
          best = score
          val path = visited.reverse
          val pathIds = formatPath(path)
          report(score, pathIds)
        }
        (score, visited.reverse)
      } else {
        candidatesForNextStop.take(3).map { case (candidate, distance) =>
//          if (badEdges contains (current -> candidate)) {
//            (score, visited.reverse)
//          } else {
            val newVisited = candidate :: visited
            loop(candidate, energy + current.energy - pow(distance, 2), score + distance, newVisited, notVisited - candidate)
//          }
        }.maxBy(_._1)
      }
    }

    stops.sortBy(-1 * _.energy).take(10).par.map { start =>
      val partitionResult = loop(start, energy = 0, score = 0, List(start), stops.toSet - start)
      println(s"PARTITION ${start.label}: $partitionResult")
      partitionResult
    }.maxBy(_._1)
  }

  def reachable(energy: Double, stop: Stop, candidates: Set[Stop]): List[(Stop, Double)] = {
    candidates.map(candidate => candidate -> distanceBetween(stop, candidate)).filter { case (_, distance) =>
      pow(distance, 2) <= energy
    }.toList.sortBy(-1 * _._2)
  }

  def distanceBetween(stop1: Stop, stop2: Stop): Double = {
    sqrt(pow(abs(stop1.x - stop2.x), 2) + pow(abs(stop1.y - stop2.y), 2))
  }

  def doubleCheck(path: List[Stop]): Double = {
    path.sliding(2).foldLeft(0D) { case (total, prev :: next :: Nil) =>
      total + distanceBetween(prev, next)
    }
  }

  def formatPath(stops: List[Stop]): String = {
    stops.map(_.label).mkString(",")
  }


  def allEdges[A](stops: List[A]): Set[(A, A)] = {
    def loop(acc: Set[(A, A)], remaining: List[A]): Set[(A, A)] = {
      remaining match {
        case Nil => acc
        case a :: tail =>
          loop(acc ++ tail.map(a -> _).toSet, tail)
      }
    }
    loop(Set.empty, stops)
  }

  def longEdges(edges: Set[(Stop, Stop)]): Set[(Stop, Stop)] = {
    val oneDirection = edges.toList.sortBy { case (stop1, stop2) =>
      -1 * distanceBetween(stop1, stop2)
    }.take(edges.size / 2).toSet

    oneDirection.flatMap { case (stop1, stop2) =>
      Set(stop1 -> stop2, stop2 -> stop1)
    }
  }
}
