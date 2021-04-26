import { solution } from 'sixkyu/roman_numerals_decoder';
import { assert } from 'chai';

describe("solution", function() {
    it('tests MMVIII', () => {
        const actual = solution('MMVIII');
        assert.equal(actual, 2008);
    });

    it('tests MCMXC', () => {
        const actual = solution('MCMXC');
        assert.equal(actual, 1990);
    });

    it('tests MDCLXVI', () => {
        const actual = solution('MDCLXVI');
        assert.equal(actual, 1666);
    });

    it('tests X', () => {
        const actual = solution('X');
        assert.equal(actual, 10);
    });
});