function generatePrime(a, b) { //a is "starting number", b is "how many".
  "use strict";
  var e = [],
    pC = function (g) {
      var limit = Math.floor(Math.sqrt(g)),
        mod = 2,
        output = 1;
      while (mod <= limit) {
        if (g % mod === 0) {
          output = 0;
          break;
        }
        mod += 1;
      }
      return output;
    };
  a = Number(a);
  b = Number(b);
  if ((a > 1 && b > 0) && (a % 1 === 0) && (b % 1 === 0)) {
    a += 1; // so that the starting number isn't included.
    while (b > 0) {
      if (pC(a)) {
        e.push(a);
        b -= 1;
      }
      a += 1;
    }
  } else {
    e.push("Error: a > 1 && b > 0 && (a & b) positive integers.");
  }
  return e;
}