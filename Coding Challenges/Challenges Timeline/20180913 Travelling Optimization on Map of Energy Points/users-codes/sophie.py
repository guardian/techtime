import requests
import math
from itertools import combinations
from datetime import datetime
import time
import random
from random import shuffle

ipAddress = "http://10.249.16.173:14462"

class MapPoint:
	def __init__(self, jsonDictionary):
		self.label = jsonDictionary["label"]
		self.x = jsonDictionary["coordinates"][0]
		self.y = jsonDictionary["coordinates"][1]
		self.energy = jsonDictionary["energy"]


class GameMap:
	def __init__(self, jsonDictionary):
		self.id = jsonDictionary["mapId"]
		self.points = map(lambda p: MapPoint(p), jsonDictionary["points"])
		self.generateDistanceMatrix()

	def generateDistanceMatrix(self):
		self.distanceMatrix = {}
		for pointA in self.points:
			self.distanceMatrix[pointA.label] = {}
			distancesFromA = {}
			for pointB in self.points:
				self.distanceMatrix[pointA.label][pointB.label] = math.sqrt((pointA.x - pointB.x)**2 + (pointA.y - pointB.y)**2 )

	def distance(self, pointA, pointB):
		return self.distanceMatrix[pointA.label][pointB.label]


class Route:
	def __init__(self, gameMap, points):
		self.gameMap = gameMap
		self.points = points

		self.distance = 0

		energy = 0
		for i in range(0, len(points) - 1): # Stop when we get to the last point (no circular routes)
			pointA = self.points[i]
			pointB = self.points[i + 1]

			energy += pointA
			distanceToB = self.gameMap.distance(pointA, pointB)
			energyNeeded = distanceToB**2 # Energy required to get to next point is the square of the distance
			if energy >= energyNeeded:
				self.distance += distanceToB
				energy -= energyNeeded
			else:
				# We've run out of energy, end of the line
				break

	def __str__(self):
		return ",".join(point.label for point in self.points)


def submit(route):
	request = "{}/game/v1/submit/sophie.lambrakis/{}/{}".format(ipAddress, route.gameMap.id, str(route))
	print(request)
	try:
		response = requests.get(request)
		print(response.text)
		print(response.json())
	except:
		pass	


lastMap = None
while True:
	try:
		mapData = requests.get(ipAddress + '/game/v1/map').json()
		gameMap = GameMap(mapData)

		if lastMap != None and lastMap.id == gameMap.id:
			# Try every 30 seconds until we get a new map
			time.sleep(30)
			continue

		print("----- NEW MAP -----")
		print(datetime.now())
		print(mapData)
		lastMap = gameMap

		highestDistance = 0
		while True:
			shuffle(gameMap.points)
			route = Route(gameMap, gameMap.points)
			if route.distance > highestDistance:
				highestDistance = route.distance
				submit(route)

			# If it's time for a new map, give up on this one
			if datetime.now().minute >= 58:
				print("Stop and wait for a new map")
				break
	except:
		pass	

