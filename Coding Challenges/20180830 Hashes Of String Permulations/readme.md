## Hashes Of String Permulations


### Challenge

This week's challenge is computational heavy. The task is to find a sequence `s` of distinct non empty strings 

```
s = [str_1, str_2, ..., str_n]
```

such that, for **any** permutation `p` of the sequence `s`, 

`sha1(concat(p(s)))` ends with a `0`.

To clarify: 

- `s` is a sequence of strings. 
- `p : Seq[String] -> Seq[String]` simply permutes the elements of the sequence. 
- `concat: Seq[String] -> String` is the simple concatenation operator 
- `sha1` is the standard hash function whose output is the standard hexadecimal representation (so we expect the last character to be "0").

### Scoring

You get 2 points times the length of your sequence. You will get two more points for also providing your source code (if you automated the search).

### Submission

Make a folder with your name in the challenge directory and put in your (best) sequence and optionaly your source code 

### Update, Thursday at 16:30


Change to the challenge. This change ammend and replace 

Either you play the original challenge, variant **A**, with the slight little change that the scoring is now: 2 times the square of (the length of your sequence - 1). For instance: if your sequence has 4 elements that's 2 * (4-1)^2 = 18 points for you 🙂

Or you play the following challenge, variant *B*:

`sha1(concat(p(s)))` ends with a `0` for all permutation `p` is no longer a requirement for a sequence to be eligible. Instead here is how I am going to compute the score. 

1. You give me any (finite) sequence of strings, of length no more than 10.
2. I will compute the ratio of permutations for which `sha1(concat(p(s)))` ends with a `0`, a number between 0 and 1.
3. The score will be 2 times that ratio times the length of your sequence. 

In your submittion specify whether you have been playing variant *A* or variant *B*. 

