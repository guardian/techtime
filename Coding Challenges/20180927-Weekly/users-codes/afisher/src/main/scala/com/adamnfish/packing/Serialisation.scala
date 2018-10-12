package com.adamnfish.packing

import io.circe._
import io.circe.generic.semiauto.deriveDecoder


object Serialisation {

  implicit val mapDecoder: Decoder[RawMap] = deriveDecoder[RawMap]
  implicit val pointDecoder: Decoder[RawPoint] = deriveDecoder[RawPoint]

  def rawMapToMap(rawMap: RawMap): Map = {
    Map(
      rawMap.mapId,
      rawMap.timestamp,
      rawMap.points.map(rawPointToPoint)
    )
  }

  def rawPointToPoint(rawPoint: RawPoint): Point = {
    Point(
      label = rawPoint.label,
      x = rawPoint.coordinates(0),
      y = rawPoint.coordinates(1)
    )
  }
}
