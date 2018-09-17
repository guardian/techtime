const { hashes, score, perms } = require('./utils');
const { tco } = require('./tco');

const itos = i => i.toString(36);

const run = tco((len = 1, prevStrs = [], i = 0, max = { score: -Infinity }) => {
  const strs = prevStrs.concat(itos(++i)).slice(-len);
  const s = score(strs);
  if (s > max.score) {
    const newMax = { arr: strs, score: s };
    console.log(`
    --- New max ---
    ${JSON.stringify(newMax)}
    `);
    return [run, [s === strs.length * 2 ? len + 1 : len, strs, i + 1, newMax]];
  }
  return [run, [len, strs, i + 1, max]];
});

run();
