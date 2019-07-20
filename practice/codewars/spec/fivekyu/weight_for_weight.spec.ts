import {WeightForWeight} from 'fivekyu/weight_for_weight';
import {assert} from "chai";

describe("Fixed Tests", function () {
    it("orderWeight", function () {
        assert(WeightForWeight.orderWeight("103 123 4444 99 2000") === "2000 103 123 4444 99");
        assert(WeightForWeight.orderWeight("2000 10003 1234000 44444444 9999 11 11 22 123") === "11 11 2000 10003 22 123 1234000 44444444 9999");
    });
});