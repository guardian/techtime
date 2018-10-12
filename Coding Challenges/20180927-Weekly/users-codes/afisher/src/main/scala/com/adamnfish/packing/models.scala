package com.adamnfish.packing

case class RawMap(mapId: String, timestamp: String, points: List[RawPoint])
case class RawPoint(label: String, coordinates: List[Double])

case class Map(mapId: String, timestamp: String, points: List[Point])
case class Point(label: String, x: Double, y: Double)

case class Disc(point: Point, radius: Double) {
  val stringRepr = s"${point.label},$radius"
}
