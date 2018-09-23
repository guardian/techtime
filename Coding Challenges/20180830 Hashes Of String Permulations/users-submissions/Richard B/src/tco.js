const tco = f1 => {
  const tf = (...args1) => {
    let f = f1;
    let args = args1;
    while (true) {
      const [f2, args2] = f(...args);
      if (typeof f2 === 'function') {
        f = f2.tco;
        args = args2;
      } else {
        return args2;
      }
    }
  };
  tf.tco = f1;
  return tf;
};

module.exports = {
  tco
};
