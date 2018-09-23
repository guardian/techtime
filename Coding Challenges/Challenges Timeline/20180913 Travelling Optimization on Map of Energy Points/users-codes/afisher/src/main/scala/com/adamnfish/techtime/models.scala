package com.adamnfish.techtime


case class Map(mapId: String, timestamp: String, points: List[Point])

case class Point(label: String, coordinates: List[Double], energy: Double)
case class Stop(label: String, x: Double, y: Double, energy: Double)
