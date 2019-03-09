import solution = require('fourkyu/magnet_particules_in_boxes');
import {assert} from "chai";

function assertFuzzyEquals(actual: number, expected: number){
  let inrange = Math.abs(actual - expected) <= 1e-6;
  assert.equal(inrange, true, "Expected Math.abs(actual - expected) <= 1e-6. " + expected +", got " + actual);
}
function testing(maxk: number, maxn: number, expected: number) {
  assertFuzzyEquals(solution.G964.doubles(maxk, maxn), expected);
}
describe("Fixed Tests magnets", function() {
  it("Basic tests", function() {
    testing(1, 10, 0.5580321939764581);
    testing(10, 1000, 0.6921486500921933);
    testing(10, 10000, 0.6930471674194457);
  });
});