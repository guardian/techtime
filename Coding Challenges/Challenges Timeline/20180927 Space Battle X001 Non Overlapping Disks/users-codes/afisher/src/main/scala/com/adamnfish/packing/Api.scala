package com.adamnfish.packing

import java.util.concurrent.Executors

import cats.effect.IO
import com.adamnfish.packing.Serialisation._
import io.circe.Decoder.Result
import io.circe.Json
import lol.http.{Client, Get, Response}
import lol.json._

import scala.concurrent.ExecutionContext


object Api {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(200))
  private val gameClient = Client("10.249.16.173", 14561, "http")(ec)

  def getMap(): IO[Result[RawMap]] = {
    for {
      map <- gameClient.run(Get("/challenge-20180927/map")) {
        _.readSuccessAs[Json].map(_.as[RawMap])
      }
    } yield map
  }

  def submitAnswer(score: Double, mapId: String, discs: List[Disc]): IO[Int] = {
    val discSubmission = discs.map(_.stringRepr).mkString(",")
    val url = s"/challenge-20180927/submit/$mapId/adam.fisher/$discSubmission"

    for {
      statusCode <- gameClient.run(Get(url)) { response: Response =>
        if (response.status >= 400 && response.status != 409) {
          println(s"ERROR: ${response.status} for $url")
        }
        IO.pure(response.status)
      }
    } yield statusCode
  }
}
