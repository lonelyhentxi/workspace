const scoreRates: { [key: string]: number } = {
  Wild: 10,
  Star: 9,
  Bell: 8,
  Shell: 7,
  Seven: 6,
  Cherry: 5,
  Bar: 4,
  King: 3,
  Queen: 2,
  Jack: 1
};

export function fruit(reels: string[][], spins: number[]): number {
  const results: { [key: string]: number } = {};
  spins.forEach((spin, index) => {
    const value = reels[index][spin];
    if (results[value] === undefined) {
      results[value] = 1;
    } else {
      results[value] = results[value] + 1;
    }
  });
  let elems: [string, number][] = [];
  for (const k in results) {
    elems.push([k, results[k]]);
  }
  elems = elems.sort((a, b) => b[1] - a[1]);
  if (elems.length === 1) {
    const main = elems[0][0];
    return scoreRates[main] * 10;
  } else if (elems.length == 2) {
    return scoreRates[elems[0][0]] * (elems[1][0] === "Wild" ? 2 : 1);
  } else {
    return 0;
  }
}
