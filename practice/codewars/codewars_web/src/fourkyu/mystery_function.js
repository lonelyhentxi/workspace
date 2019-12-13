function mystery(n) {
  const length = n===0? 1: Math.ceil(Math.log2(n + 1));
  const res = []
  let current = n;
  for(let i=length-1;i>=0;i--) {
    const factor = Math.pow(2,i);
    const firstBit = Math.floor(current / factor);
    res.push(firstBit)
    current -= (firstBit * factor)
    if(firstBit===1) {
        current = factor - 1 - current;
    }
  }
  return parseInt(res.join(''),2);
}

function mysteryInv(n) {
  const bits = [...n.toString(2).split('')].map(d=>parseInt(d));
  let sum = 0;
  for(let i=0;i<bits.length;i++) {
      const factor = Math.pow(2,i);
      const thisBit = bits[bits.length - 1 - i];
      if(thisBit===1) {
          sum = factor - 1 - sum;
      }
      sum += thisBit * factor;
  }
  return sum;
}

function nameOfMystery(n) {
    return "Gray code";
}

exports.mystery = mystery;
exports.nameOfMystery = nameOfMystery;
exports.mysteryInv = mysteryInv;