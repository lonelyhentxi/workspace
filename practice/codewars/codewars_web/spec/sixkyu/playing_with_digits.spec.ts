import { PlayingWithDigits } from 'sixkyu/playing_with_digits';
import { assert } from 'chai';

describe("Fixed Tests", function() {
    it("digPow", function() {
        assert( PlayingWithDigits.digPow(89, 1)===1);
        assert( PlayingWithDigits.digPow(92, 1)=== -1);
        assert( PlayingWithDigits.digPow(114, 3)=== 9);
    });
});