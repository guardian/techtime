#!/usr/bin/env node

const { infixToRPN } = require('./');
console.log(infixToRPN(process.argv[2] || ""))
