import * as crypto from "crypto";

type Sequence = Array<string>;

const getSha1 = (input: string) =>
  crypto
    .createHash("sha1")
    .update(input)
    .digest("hex");

const getRandomString = (length: number) =>
  crypto.randomBytes(length).toString("hex");

const withoutIndex = <T>(arr: Array<T>, index: number) =>
  arr.slice(0, index).concat(arr.slice(index + 1));

const permutations: <T>(seq: Array<T>) => T[][] = seq =>
  seq.reduce((acc, el, index, arr) => {
    if (!arr.length) return [[]];
    if (arr.length === 1) return [arr];
    return [
      ...acc,
      ...permutations(withoutIndex(arr, index)).reduce(
        (acc, perms) => [...acc, [el, ...perms]],
        []
      )
    ];
  }, []);

const isSequenceValid = (s: Sequence) => getSha1(s.join("")).slice(-1) === "0";

let currentLength = 1;
let iterations = 0;

while (true) {
  const sequence: Sequence = new Array(currentLength);
  for (let i = 0; i < currentLength; i++) {
    sequence[i] = getRandomString(5);
  }
  const sequencePermutations = permutations(sequence);
  if (sequencePermutations.every(isSequenceValid)) {
    console.log(
      `Found valid sequence of length ${currentLength} at iteration ${iterations}: `,
      sequence
    );
    currentLength++;
    iterations = 0;
  }
  iterations++;
}

