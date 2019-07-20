import { assert } from 'chai';
import { isSolved } from 'fivekyu/tic_tac_toe_checker';

describe('tic tac toe checks',()=>{
    it('should return -1',()=>{
        assert(isSolved([[0,0,1],
            [0,1,2],
            [2,1,0]]) === -1);
    });
});