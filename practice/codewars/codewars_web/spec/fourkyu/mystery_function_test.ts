import { mystery, mysteryInv } from 'fourkyu/mystery_function';
import { assert } from "chai";

describe("Fixed Tests magnets", function () {
    it("Basic tests", function () {
        assert.equal(mystery(6), 5, "mystery(6)");
        assert.equal(mysteryInv(5), 6, "mysteryInv(5)");
        assert.equal(mystery(9), 13, "mystery(9)");
        assert.equal(mysteryInv(13), 9, "mysteryInv(13)");
        assert.equal(mystery(19), 26, "mystery(19)");
        assert.equal(mysteryInv(26), 19, "mysteryInv(26)");
    });
});