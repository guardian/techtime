const subs = ['(', ')'];
const bins = ['+', '-', '*', '/', '^'];

const removeWhiteSpace = str => str.replace(/\s/g, '');

const unpack = n =>
  n instanceof Array ? (typeof n[0] === 'string' ? n.join('') : n[0]) : n;

const binarizeLevel = level =>
  unpack(
    bins.reduce((lvl, operator) => {
      const index = lvl instanceof Array ? lvl.indexOf(operator) : -1;
      if (index > -1) {
        const pre = lvl.slice(0, index);
        const post = lvl.slice(index + 1);
        return {
          operator,
          left: binarizeLevel(pre),
          right: binarizeLevel(post)
        };
      }
      return lvl;
    }, level)
  );

const ast = (str, prev = []) => {
  const char = str.charAt(0);
  const next = str.slice(1);
  switch (char) {
    case subs[0]: {
      const [left, children] = ast(next);
      return ast(left, [...prev, children]);
    }
    case '':
    case subs[1]: {
      return [next, binarizeLevel(prev)];
    }
  }
  return ast(next, [...prev, char]);
};

const rpn = t =>
  typeof t === 'string' ? t : `${rpn(t.left)} ${rpn(t.right)} ${t.operator}`;

const infixToRPN = str => {
  const [, tree] = ast(removeWhiteSpace(str));
  return rpn(tree);
};

module.exports = { infixToRPN };
