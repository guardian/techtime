package com.adamnfish.techtime

import java.util.concurrent.Executors

import cats.effect.IO
import com.adamnfish.techtime.Serialisation._
import io.circe.Decoder.Result
import io.circe.Json
import lol.http.{Client, Get, Response}
import lol.json._

import scala.concurrent.ExecutionContext


object Api {
  var best = 0D

  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(200))
  private val gameClient = Client("10.249.16.173", 14462, "http")(ec)

  def getMap(): IO[Result[Map]] = {
    for {
      map <- gameClient.run(Get("/game/v1/map")) {
        _.readSuccessAs[Json].map(_.as[Map])
      }
    } yield map
  }

  def submitAnswer(score: Double, mapId: String, pathIds: String): IO[Int] = {
    synchronized {
      if (score > best) {
        best = score

        val url = s"/game/v1/submit/adam.fisher/$mapId/$pathIds"

        for {
          statusCode <- gameClient.run(Get(url)) { response: Response =>
            if (response.status >= 400 && response.status != 409) {
              println(s"ERROR: ${response.status} for $url")
            }
            IO.pure(response.status)
          }
        } yield statusCode
      } else {
        println(s"$score was not better than $best")
        IO.pure(208)
      }
    }
  }
}
