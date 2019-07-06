import * as solution from 'sevenkyu/maximum_length_difference';
import {assert} from "chai";

describe("Fixed Tests", function() {
    it("Basic tests mxdiflg", function() {

        var s1 = ["hoqq", "bbllkw", "oox", "ejjuyyy", "plmiis", "xxxzgpsssa", "xxwwkktt", "znnnnfqknaz", "qqquuhii", "dvvvwz"];
        var s2 = ["cccooommaaqqoxii", "gggqaffhhh", "tttoowwwmmww"];
        assert.equal(solution.G964.mxdiflg(s1, s2), 13);
        s1 = ["ejjjjmmtthh", "zxxuueeg", "aanlljrrrxx", "dqqqaaabbb", "oocccffuucccjjjkkkjyyyeehh"];
        s2 = ["bbbaaayddqbbrrrv"];
        assert.equal(solution.G964.mxdiflg(s1, s2), 10);
    });
});
