# This code deliberately left ugly/untidied and inefficient,
# as a reminder that sometimes it's OK to just hack together a prototype.

import math

def sophieNum(i):
	sophieList = []
	for c in str(i):
		sophieList.append(int(c)**2)
	sophie = ''.join(map(str, sophieList))
	return sophie

def isSquare(i):
	return math.sqrt(float(i)).is_integer()

for i in range(1000, 10000):
	if i % 10 != 0 and isSquare(sophieNum(i)):
		print "Success!"
		print i
		break