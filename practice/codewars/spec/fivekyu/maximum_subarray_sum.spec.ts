import { assert } from "chai";
import { maxSequence } from "fivekyu/maximum_subarray_sum";

describe("maxSequence", function() {
  it("should work on an empty array", function() {
    assert(maxSequence([]) === 0);
  });
  it("should work on the example", function() {
    assert(maxSequence([-2, 1, -3, 4, -1, 2, 1, -5, 4]) === 6);
  });
});
