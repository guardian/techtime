
### Challenge

This week's challenge is computational heavy. The task is to find a sequence `s` of distinct non empty strings 

```
s = [str_1, str_2, ..., str_n]
```

sucj that, for **any** permutation `p` of the sequence `s`, 

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

