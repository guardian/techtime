
Today's challenge is inspired by [https://xkcd.com/173/](https://xkcd.com/173/) (Movie Seating)

Instead of people, we consider integers from 0 to 99. The task is to put those numbers in a sequence so that a given number is maximised.

The following definitions are what is required to define/compute that number. 

### Trace of a number

Given n integer `n`, for instance 12, we consider its sha1 hash, for 12 that would be "7b52009b64fd0a2a49e6d8a939753077792b0554".

```
$ echo -n "12" | openssl sha1
7b52009b64fd0a2a49e6d8a939753077792b0554
```

We remove all the digits from that hash, to get `bbfdaaedab`, which is defined as the **trace** of 12, denoted `trace(12)`.

```
trace(n: Int): String 
```

### Affinity of two numbers 

Given two numbers `n` and `m`, we define the affinity of the pair, denoted `affinity(n, m)` as the Levenshtein distance between their traces. More exactly 

```
affinity(n: Int, m: Int): Int 
affinity(n, m) = Levenshtein(trace(n), trace(m))
```

### Positonal distance between two numbers in the sequence.

When the numbers are sequenced, for instance, 

```
[ ..., 12, 54, 89, 91, ... ]
```

Then 12 and 54, are at distance 1 of each other, 12 and 89 are at distance 2 etc. This distance is denoted `posdist`, for instance `posdist(12, 91) = 3`.

```
posdist(n: Int, m: Int, sequence: List[Int]): Int 
```

I just wrote `posdist` as taking 3 arguments, the third one being the actual sequence only to make it a pure function.

### Pair Score

Given a sequence (meaning a particular ordering of the numbers, from 1 to 99), and given two of those numbers (distinct) `n` and `m`, we define `pairScore(n, m)` by 

```
pairScore(n: Int, m: Int, sequence: List[Int]): Int =
	affinity(n, m).toDouble/posdist(n, m, sequence)
```

Meaning their affinity divided by their distance.

### Sequence Score

Given a sequence, we define `score(sequence)` by `sum` of `pairScore(n, m, sequence)` where

```
 0 <= n <= 99
 0 <= m <= 99
 n < m
 posdist(n, m, sequence) <= 3
```

### Challenge

The challenge is to find a sequence that maximises the sequence score.

### How to submit a solution

This challenge comes with a strict deadline. **Monday 23rd at 6pm**.

To submit your solution, just email your sequence to Pascal (comma separated sequence of numbers). Email must arrive before the deadline.

An example of sequence is: 56, 44, 7, 18, 6, 61, 98, 4, 21, 64, 58, 81, 78, 22, 93, 55, 79, 24, 86, 13, 51, 57, 90, 54, 92, 67, 8, 88, 3, 36, 84, 99, 77, 1, 37, 38, 14, 29, 50, 60, 35, 42, 70, 73, 27, 53, 46, 83, 23, 52, 94, 63, 96, 33, 12, 31, 91, 69, 0, 28, 80, 30, 40, 41, 34, 75, 71, 89, 87, 65, 9, 62, 82, 11, 48, 17, 49, 39, 16, 5, 74, 25, 95, 2, 85, 10, 76, 32, 59, 66, 43, 45, 47, 20, 15, 72, 68, 97, 19, 26 

### Points

Once all the { solutions / sequences } are in, I will compute the score of each, put them in decreasing value, and the higest scored sequence gets 6 points, the next one gets 70% of that, and the next one 70% of that etc... In other words the `n^{th}` ranked solution gets `6 * 0.7^(n-1)` points. 

If there are no submittion by the deadline, **and only in that case**, then the first ever submission after the deadline gets 6 points.

