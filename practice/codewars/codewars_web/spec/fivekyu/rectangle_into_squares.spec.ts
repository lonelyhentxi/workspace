import solution = require('fivekyu/rectangle_into_squares');
import {assert} from "chai";

describe("Fixed Tests sqInRect", function () {
    it("Basic tests", () => {
        assert.deepEqual(
            solution.G964.sqInRect(5, 5),
            null);
        assert.deepEqual(
            solution.G964.sqInRect(5, 3),
            [3, 2, 1, 1])
    });
});