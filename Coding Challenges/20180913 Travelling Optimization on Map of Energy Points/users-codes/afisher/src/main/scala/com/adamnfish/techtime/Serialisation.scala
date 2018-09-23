package com.adamnfish.techtime

import io.circe._
import io.circe.generic.semiauto.deriveDecoder


object Serialisation {

  implicit val mapDecoder: Decoder[Map] = deriveDecoder[Map]
  implicit val pointDecoder: Decoder[Point] = deriveDecoder[Point]

  def pointToStop(point: Point): Stop = {
    Stop(
      label = point.label,
      x = point.coordinates(0),
      y = point.coordinates(1),
      energy = point.energy
    )
  }
}
