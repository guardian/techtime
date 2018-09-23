const crypto = require('crypto');

const perms = (arr, base = []) =>
  arr.length
    ? arr.reduce(
        (acc, el, i) => [
          ...acc,
          ...perms([...arr.slice(0, i), ...arr.slice(i + 1)], [...base, el])
        ],
        []
      )
    : [base];

const concat = arr => arr.join('');

const sha1 = str =>
  crypto
    .createHash('sha1')
    .update(str)
    .digest('hex');

const pipe = (...fns) => x => fns.reduce((acc, fn) => fn(acc), x);
const endsWithZero = str => str.slice(-1) === '0';
const extractSum = fn => (pre, el) => pre + fn(el);

const hashes = arr =>
  perms(arr).map(
    pipe(
      concat,
      sha1
    )
  );

const score = arr => {
  const arr2 = hashes(arr);
  const ratio = arr2.reduce(extractSum(endsWithZero), 0) / arr2.length;
  return 2 * ratio * arr.length;
};

module.exports = {
  hashes,
  score
};
