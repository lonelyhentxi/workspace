import {Challenge} from 'sixkyu/valid_braces';
import {assert} from "chai";

function test(str: string, expected: boolean) {
    const actual = Challenge.validBraces(str);
    assert.strictEqual(actual, expected, `Expected ${str} to evaluate to be ${expected}. Got ${actual}`)
}

function expect_valid(str: string) {
    test(str, true)
}

function expect_invalid(str: string) {
    test(str, false)
}

describe("solution", function () {
    it("should handle basic tests", function () {
        expect_valid("()");
        expect_invalid("[(])");
    });
});