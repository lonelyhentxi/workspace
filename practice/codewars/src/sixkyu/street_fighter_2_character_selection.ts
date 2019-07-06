const moveMotivations: { [key: string]: [number, number] } = {
  up: [-1, 0],
  down: [1, 0],
  left: [0, -1],
  right: [0, 1]
};

export function streetFighterSelection(
  fighters: Array<string[]>,
  position: number[],
  moves: string[]
): string[] {
  let [x, y] = position;
  const rows = fighters.length;
  const cols = fighters[0].length;
  const roleNames = [];
  for (const literalMove of moves) {
    const [verMov, horMov] = moveMotivations[literalMove];
    const nextX = verMov + x;
    if (nextX >= 0 && nextX < rows) {
      x = nextX;
    }
    y = (horMov + y + cols) % cols;
    roleNames.push(fighters[x][y]);
  }
  return roleNames;
}
