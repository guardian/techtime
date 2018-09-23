# sha1seq

Searches the set of string for sequences of length 3 or 4 that satisfy the following predicate:

  > let xs_1, ..., xs_n be strings and P(xs_1, ..., xs_n) be the set of permutations of these strings, for all p in P, we have sha1(concat(p)) ends with 0

A constructive proof will quickly explode in terms of complexity, but we can slash it down drastically by using the following heuristic:

  > It is easy to find a string s of length >= 4 for which sha1(s) ends with 0

From there, we can generate all ordered n-uples and for each one generate all the permutations to test the predicate above. In all these permutations, we know
that the first will always match since it yields the original string s.

It takes a couple of seconds to find a sequence of length 3. This one for example:

```
"pqtd"
"wkazwyilvtzwprvgwsbjsenlkbpwmxjyknurtnzknncyaewkr"
"gaauhyaubshylegeviziuxxhbyxiiqhczllzkgagqmzgkgn"
```

To run this program, just type `stack exec sha1seq-exe`.