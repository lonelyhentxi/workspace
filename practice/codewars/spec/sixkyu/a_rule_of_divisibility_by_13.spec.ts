import { assert } from "chai";
import { ARuleOfDivisibilityBy13 } from "sixkyu/a_rule_of_divisibility_by_13";

const { thirt } = ARuleOfDivisibilityBy13;

describe("Fixed Tests thirt", () => {
  it("Basic test", () => {
    assert(thirt(8529) === 79);
    assert(thirt(85299258) === 31);
  });
});
