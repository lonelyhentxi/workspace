import { computePrefixTable } from './string_match';


test('computePrefixTable',()=>{
  expect(computePrefixTable("abcdabd")).toEqual([-1,0,0,0,0,1,2]);
});