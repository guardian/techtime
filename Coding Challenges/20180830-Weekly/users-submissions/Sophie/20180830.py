from itertools import permutations
from hashlib import sha1
import string
import random

# Find a sequence s of distinct non empty strings s = [str1, str2, ..., strn] length < 10
# Such that for many permutations p of the sequence s
# sha1(concat(p(s))) ends with a 0

def permutationIsValid(p):
	return sha1("".join(p)).hexdigest()[-1] == '0'

def allPermutationsAreValid(s):
	for p in permutations(s):
		if not permutationIsValid(p):
			return False
	return True

def randomString():
	length = random.randint(1, 100)
	return ''.join(random.choice(string.ascii_letters + string.digits) for m in range(length))

while True:
	s = [randomString(), randomString(), randomString()]
	if allPermutationsAreValid(s):
		print("All permutations of sequence {} are valid".format(s))
		break
